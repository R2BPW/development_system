"""Аналитический движок: LLM-запросы и форматирование результата."""
import json
import os
import re
import httpx

from .prompts import SYSTEM, COMPENSATION_PROMPT, EDGE_CASES_PROMPT, REACHABILITY_PROMPT
from .parser import (
    Node, Edge, graph_summary, find_unreachable,
    extract_error_paths, models_summary
)

MODEL = os.environ.get("ANALYZER_MODEL", "anthropic/claude-opus-4-5")
API_URL = "https://openrouter.ai/api/v1/chat/completions"


def _llm(prompt: str) -> str:
    key = os.environ.get("OPENROUTER_API_KEY", "")
    resp = httpx.post(
        API_URL,
        headers={
            "Authorization": f"Bearer {key}",
            "Content-Type": "application/json",
        },
        json={
            "model": MODEL,
            "messages": [
                {"role": "system", "content": SYSTEM},
                {"role": "user",   "content": prompt},
            ],
            "max_tokens": 2000,
        },
        timeout=90,
    )
    resp.raise_for_status()
    return resp.json()["choices"][0]["message"]["content"]


def _parse_json_findings(text: str) -> list[dict]:
    """Извлекает findings из ответа LLM (даже если обёрнут в markdown)."""
    # Убрать ```json ... ```
    text = re.sub(r'```(?:json)?\s*', '', text).strip()
    text = text.rstrip('`').strip()
    try:
        data = json.loads(text)
        return data.get("findings", [])
    except json.JSONDecodeError:
        # Попробовать найти JSON-объект внутри текста
        m = re.search(r'\{.*"findings".*\}', text, re.DOTALL)
        if m:
            try:
                return json.loads(m.group())["findings"]
            except Exception:
                pass
    return []


def _format_error_paths(error_paths: list[dict]) -> str:
    lines = []
    for ep in error_paths:
        node = ep['error_node']
        preceding = ep['preceding_steps']
        steps = ', '.join(f"{n.id}[{n.label[:30]}]" for n in preceding)
        lines.append(f"  {node.id}[{node.label}] ← предшествующие: {steps or 'нет данных'}")
    return '\n'.join(lines) if lines else "error-узлы не найдены"


def analyze(
    graph_text: str,
    nodes: dict[str, Node],
    edges: list[Edge],
    models_text: str,
    models_parsed: list[dict],
    verbose: bool = True,
) -> dict:
    """
    Запускает все аналитические шаги и возвращает объединённый результат.
    """
    graph_desc = graph_summary(nodes, edges)
    models_desc = models_summary(models_parsed)
    unreachable = find_unreachable(nodes, edges)
    error_paths = extract_error_paths(nodes, edges)
    error_paths_desc = _format_error_paths(error_paths)
    unreachable_desc = (
        ', '.join(f"{n.id}[{n.label[:30]}]" for n in unreachable)
        if unreachable else "недостижимых узлов не обнаружено"
    )

    all_findings = []
    steps = [
        ("Пробелы компенсации",    COMPENSATION_PROMPT, {
            "graph": graph_desc,
            "models": models_desc,
            "error_paths": error_paths_desc,
        }),
        ("Крайние случаи",         EDGE_CASES_PROMPT, {
            "graph": graph_desc,
            "models": models_desc,
        }),
        ("Достижимость",           REACHABILITY_PROMPT, {
            "graph": graph_desc,
            "unreachable": unreachable_desc,
        }),
    ]

    for step_name, prompt_tpl, ctx in steps:
        if verbose:
            print(f"  [{step_name}] запрос к LLM...", flush=True)
        try:
            prompt = prompt_tpl.format(**ctx)
            raw = _llm(prompt)
            findings = _parse_json_findings(raw)
            for f in findings:
                f["_step"] = step_name
            all_findings.extend(findings)
            if verbose:
                print(f"  [{step_name}] найдено: {len(findings)}", flush=True)
        except Exception as e:
            if verbose:
                print(f"  [{step_name}] ОШИБКА: {e}", flush=True)

    critical = [f for f in all_findings if f.get("severity") == "critical"]
    warnings  = [f for f in all_findings if f.get("severity") == "warning"]
    info      = [f for f in all_findings if f.get("severity") == "info"]

    return {
        "total":    len(all_findings),
        "critical": len(critical),
        "warnings": len(warnings),
        "info":     len(info),
        "findings": all_findings,
        "by_category": {
            "compensation_gap":    [f for f in all_findings if f.get("category") == "compensation_gap"],
            "unexpected_state":    [f for f in all_findings if f.get("category") == "unexpected_state"],
            "edge_case":           [f for f in all_findings if f.get("category") == "edge_case"],
            "unreachable_branch":  [f for f in all_findings if f.get("category") == "unreachable_branch"],
            "other":               [f for f in all_findings
                                    if f.get("category") not in {
                                        "compensation_gap", "unexpected_state",
                                        "edge_case", "unreachable_branch"
                                    }],
        },
    }


SEVERITY_ICON = {"critical": "🔴", "warning": "⚠️ ", "info": "ℹ️ "}
CATEGORY_ICON = {
    "compensation_gap":   "🔴 Компенсация",
    "unexpected_state":   "❗ Состояние",
    "edge_case":          "⚠️  Крайний случай",
    "unreachable_branch": "🟡 Достижимость",
}


def format_report(result: dict, graph_name: str = "процесс") -> str:
    lines = [
        f"📊 Отчёт: {graph_name}",
        f"Всего находок: {result['total']}  "
        f"(🔴 критичных: {result['critical']}, ⚠️  предупреждений: {result['warnings']}, ℹ️  замечаний: {result['info']})",
        "",
    ]

    for finding in result["findings"]:
        sev   = finding.get("severity", "info")
        cat   = finding.get("category", "other")
        node  = finding.get("node", "?")
        desc  = finding.get("description", "")
        rec   = finding.get("recommendation", "")
        icon  = SEVERITY_ICON.get(sev, "ℹ️ ")
        cat_label = CATEGORY_ICON.get(cat, cat)

        lines.append(f"{icon} [{cat_label}] узел {node}")
        lines.append(f"   {desc}")
        if rec:
            lines.append(f"   → {rec}")
        lines.append("")

    return '\n'.join(lines)
