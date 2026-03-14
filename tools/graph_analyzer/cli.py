#!/usr/bin/env python3
"""
Граф-аналитик — CLI

Использование:
  python -m graph_analyzer <graph.md> <models.py>
  python -m graph_analyzer <graph.md>  # только граф, без моделей

Переменные окружения:
  OPENROUTER_API_KEY  — ключ API
  ANALYZER_MODEL      — модель (по умолчанию anthropic/claude-opus-4-5)
"""
import sys
import os

# Добавляем корень tools/ в путь
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from graph_analyzer.parser import parse_mermaid, parse_models_simple
from graph_analyzer.analyzer import analyze, format_report


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    graph_path = sys.argv[1]
    models_path = sys.argv[2] if len(sys.argv) > 2 else None

    # Читаем граф
    with open(graph_path, encoding='utf-8') as f:
        graph_text = f.read()

    # Читаем модели
    models_text = ""
    if models_path:
        with open(models_path, encoding='utf-8') as f:
            models_text = f.read()

    print(f"Парсинг графа: {graph_path}")
    nodes, edges = parse_mermaid(graph_text)
    print(f"  {len(nodes)} узлов, {len(edges)} рёбер")

    models_parsed = []
    if models_text:
        print(f"Парсинг моделей: {models_path}")
        models_parsed = parse_models_simple(models_text)
        print(f"  {len(models_parsed)} классов")

    if not os.environ.get("OPENROUTER_API_KEY"):
        print("\nERROR: OPENROUTER_API_KEY не задан")
        sys.exit(1)

    graph_name = os.path.splitext(os.path.basename(graph_path))[0]
    print(f"\nАнализ '{graph_name}'...")

    result = analyze(
        graph_text=graph_text,
        nodes=nodes,
        edges=edges,
        models_text=models_text,
        models_parsed=models_parsed,
        verbose=True,
    )

    report = format_report(result, graph_name)
    print("\n" + "="*60)
    print(report)

    # Сохранить JSON-результат
    import json
    out_path = f"{graph_name}_analysis.json"
    with open(out_path, 'w', encoding='utf-8') as f:
        json.dump(result, f, ensure_ascii=False, indent=2)
    print(f"\nJSON сохранён: {out_path}")


if __name__ == "__main__":
    main()
