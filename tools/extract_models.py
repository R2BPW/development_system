#!/usr/bin/env python3
"""
Извлекает Django-модели по именам классов из репозитория apro-new.

Использование:
  python3 extract_models.py ClassName1 ClassName2 ... > /tmp/models.py
  python3 extract_models.py WasteSiteVisitedContainers AddTourTaskExecutionLog

Ищет классы в b3platform/ рекурсивно.
"""
import sys
import re
import os
from pathlib import Path

REPO = Path(os.environ.get("APRO_REPO", "/root/apro-new"))
SEARCH_DIRS = ["b3platform"]


def find_class(classname: str, repo: Path) -> tuple[str, str] | None:
    """Возвращает (filepath, source) для первого найденного класса."""
    pattern = re.compile(rf"^class\s+{re.escape(classname)}\s*[\(:]", re.MULTILINE)

    for search_dir in SEARCH_DIRS:
        for path in (repo / search_dir).rglob("*.py"):
            try:
                text = path.read_text(encoding="utf-8")
            except Exception:
                continue

            m = pattern.search(text)
            if not m:
                continue

            # Взять от начала класса до следующего класса верхнего уровня
            start = m.start()
            rest = text[start:]
            end_m = re.search(r"^class\s+\w+", rest[1:], re.MULTILINE)
            chunk = rest[: end_m.start() + 1] if end_m else rest[:4000]

            # Обрезать до ~2500 символов, не рвя строку
            if len(chunk) > 2500:
                cutoff = chunk.rfind("\n", 0, 2500)
                chunk = chunk[:cutoff] + "\n    # ... (обрезано)\n"

            return str(path.relative_to(repo)), chunk

    return None


def main():
    names = sys.argv[1:]
    if not names:
        print(__doc__)
        sys.exit(1)

    print(f"# Модели извлечены из {REPO}")
    print(f"# Классы: {', '.join(names)}\n")

    for name in names:
        result = find_class(name, REPO)
        if result:
            filepath, source = result
            print(f"# --- {filepath} ---")
            print(source)
            print()
        else:
            print(f"# WARN: класс {name} не найден в {REPO}", file=sys.stderr)


if __name__ == "__main__":
    main()
