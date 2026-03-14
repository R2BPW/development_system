"""Парсеры Mermaid-графа и Django-моделей."""
import re
from dataclasses import dataclass, field
from typing import Optional


@dataclass
class Node:
    id: str
    label: str
    is_error: bool = False
    is_terminal: bool = False
    is_decision: bool = False

    def __repr__(self):
        flags = []
        if self.is_error:    flags.append("ERR")
        if self.is_terminal: flags.append("TERM")
        if self.is_decision: flags.append("DECISION")
        return f"Node({self.id!r}, {self.label[:30]!r}{', ' + '|'.join(flags) if flags else ''})"


@dataclass
class Edge:
    from_id: str
    to_id: str
    condition: Optional[str] = None

    def __repr__(self):
        cond = f" [{self.condition}]" if self.condition else ""
        return f"{self.from_id} -->{cond} {self.to_id}"


def parse_mermaid(text: str) -> tuple[dict[str, Node], list[Edge]]:
    """Парсит Mermaid flowchart → (nodes, edges)."""
    nodes: dict[str, Node] = {}
    edges: list[Edge] = []

    # Удаляем комментарии и директивы
    lines = [l for l in text.splitlines()
             if not l.strip().startswith('%%')
             and not l.strip().startswith('```')
             and l.strip() not in ('mermaid', '')]

    body = '\n'.join(lines)

    # Узлы с прямоугольными скобками: A0["E1 Текст"]
    for m in re.finditer(r'(\w+)\["([^"]+)"\]', body):
        nid, label = m.group(1), m.group(2)
        is_err = bool(re.search(r'(ERR|ошибк|компенсац)', nid + label, re.I))
        is_term = bool(re.search(r'(финиш|completed|OK\d|успех)', label, re.I))
        nodes[nid] = Node(id=nid, label=label, is_error=is_err, is_terminal=is_term)

    # Узлы-решения: A3{"E4 Вопрос?"}
    for m in re.finditer(r'(\w+)\{"([^"]+)"\}', body):
        nid, label = m.group(1), m.group(2)
        nodes[nid] = Node(id=nid, label=label, is_decision=True)

    # Рёбра: A --> B  или  A -->|"условие"| B  или  A -- текст --> B
    for m in re.finditer(
        r'(\w+)\s*-->(?:\|"([^"]*)"\|)?\s*(\w+)', body
    ):
        edges.append(Edge(m.group(1), m.group(3), m.group(2) or None))

    return nodes, edges


def graph_summary(nodes: dict[str, Node], edges: list[Edge]) -> str:
    """Краткое текстовое описание графа для LLM."""
    lines = []

    # Узлы по типам
    error_nodes = [n for n in nodes.values() if n.is_error]
    terminal_nodes = [n for n in nodes.values() if n.is_terminal]
    decision_nodes = [n for n in nodes.values() if n.is_decision]
    regular_nodes = [n for n in nodes.values()
                     if not n.is_error and not n.is_terminal and not n.is_decision]

    lines.append(f"Граф: {len(nodes)} узлов, {len(edges)} рёбер")
    lines.append(f"  Обычные узлы ({len(regular_nodes)}): "
                 + ', '.join(f"{n.id}[{n.label[:25]}]" for n in regular_nodes))
    lines.append(f"  Решения ({len(decision_nodes)}): "
                 + ', '.join(f"{n.id}[{n.label[:25]}]" for n in decision_nodes))
    lines.append(f"  Терминальные ({len(terminal_nodes)}): "
                 + ', '.join(f"{n.id}[{n.label[:25]}]" for n in terminal_nodes))
    lines.append(f"  Ошибки ({len(error_nodes)}): "
                 + ', '.join(f"{n.id}[{n.label[:30]}]" for n in error_nodes))
    lines.append("")
    lines.append("Переходы:")
    for e in edges:
        from_label = nodes.get(e.from_id, Node(e.from_id, e.from_id)).label[:30]
        to_label = nodes.get(e.to_id, Node(e.to_id, e.to_id)).label[:30]
        cond = f" [{e.condition}]" if e.condition else ""
        lines.append(f"  {e.from_id}({from_label}){cond} --> {e.to_id}({to_label})")

    return '\n'.join(lines)


def reachable_nodes(nodes: dict[str, Node], edges: list[Edge]) -> set[str]:
    """BFS от начальных узлов (без входящих рёбер)."""
    has_incoming = {e.to_id for e in edges}
    starts = [nid for nid in nodes if nid not in has_incoming]
    if not starts:
        starts = [next(iter(nodes))]

    visited = set()
    queue = list(starts)
    adj = {}
    for e in edges:
        adj.setdefault(e.from_id, []).append(e.to_id)

    while queue:
        cur = queue.pop(0)
        if cur in visited:
            continue
        visited.add(cur)
        queue.extend(adj.get(cur, []))
    return visited


def find_unreachable(nodes: dict[str, Node], edges: list[Edge]) -> list[Node]:
    """Возвращает узлы недостижимые из начального состояния."""
    reach = reachable_nodes(nodes, edges)
    return [n for nid, n in nodes.items() if nid not in reach]


def extract_error_paths(nodes: dict[str, Node], edges: list[Edge]) -> list[dict]:
    """Для каждого error-узла — цепочка предшествующих шагов."""
    # Строим обратный граф
    rev = {}
    for e in edges:
        rev.setdefault(e.to_id, []).append(e.from_id)

    results = []
    for nid, node in nodes.items():
        if not node.is_error:
            continue
        # BFS назад на глубину 5
        path_nodes = []
        queue = [nid]
        seen = set()
        depth = 0
        while queue and depth < 5:
            next_q = []
            for cur in queue:
                if cur in seen:
                    continue
                seen.add(cur)
                if cur != nid:
                    path_nodes.append(nodes.get(cur, Node(cur, cur)))
                next_q.extend(rev.get(cur, []))
            queue = next_q
            depth += 1

        results.append({
            'error_node': node,
            'preceding_steps': path_nodes[:10],
        })
    return results


def parse_models_simple(source: str) -> list[dict]:
    """
    Простой парсер Django-моделей — извлекает классы и поля
    без AST (работает даже с неполным кодом).
    """
    models = []
    current = None

    for line in source.splitlines():
        # Новый класс
        m = re.match(r'^class\s+(\w+)\s*\(', line)
        if m:
            current = {'name': m.group(1), 'fields': [], 'raw': [line]}
            models.append(current)
            continue

        if current is None:
            continue

        current['raw'].append(line)

        # Поле модели
        m = re.match(r'\s{4}(\w+)\s*=\s*models\.(\w+)\((.{0,120})', line)
        if m:
            name, ftype, args = m.group(1), m.group(2), m.group(3)
            # Извлечь choices если есть
            choices_m = re.search(r'choices=(\w+)\.choices', args)
            null_m = 'null=True' in args
            current['fields'].append({
                'name': name,
                'type': ftype,
                'choices': choices_m.group(1) if choices_m else None,
                'nullable': null_m,
                'raw': args[:80],
            })

        # STATUS_CHOICES или аналог
        m = re.match(r'\s{4}STATUS_CHOICES\s*=\s*\[', line)
        if m:
            current.setdefault('status_choices', [])

    return models


def models_summary(models: list[dict]) -> str:
    """Компактное описание моделей для LLM."""
    lines = []
    for m in models:
        lines.append(f"\nclass {m['name']}:")
        for f in m['fields']:
            choices = f" choices={f['choices']}" if f['choices'] else ""
            null = " nullable" if f['nullable'] else ""
            lines.append(f"  {f['name']}: {f['type']}{choices}{null}")
    return '\n'.join(lines)
