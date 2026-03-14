# fix_plan.md — iOS PWA для development_system

## Цель
Добавить HTTP API к мастеру (CL) и создать React PWA для iPhone.

## Архитектура

```
iPhone Safari (PWA) ↕ HTTP REST + SSE
Hunchentoot (порт 7070) ↕ Мастер (SBCL)
```

Авторизация: Bearer token в заголовке `Authorization: Bearer <TOKEN>`.
Токен из env `WEB_API_TOKEN` (если не задан — генерировать через `(format nil "~36R" (random (expt 2 128)))` и логировать).

## Контекст проекта

- Мастер: `мастер/cl/` — Common Lisp, ASDF система `:мастер`
- Потоки: `мастер/потоки/*.lisp` — не трогать
- Функции доступны: `активные-потоки`, `запустить-поток`, `список-потоков`, `обработать-команду`
- Стиль: функции ≤ 15 строк, in-package #:мастер

## Задачи

- [x] **[HTTP-DEPS]** Обновить `мастер/cl/мастер.asd`: добавить `#:hunchentoot #:bordeaux-threads` в `:depends-on`, добавить `(:file "http")` в `:components` перед `(:file "main")`.

- [x] **[HTTP-SERVER]** Создать `мастер/cl/http.lisp`. Содержимое: (in-package #:мастер). Параметры: `*http-acceptor*` (nil), `*web-token*` (из env WEB_API_TOKEN или случайный). Функции: `start-http-server` — создаёт и запускает `hunchentoot:easy-acceptor` на порту из env `WEB_PORT` (default 7070); `stop-http-server` — останавливает акцептор. Вспомогательные: `%check-auth (request)` — проверяет заголовок Authorization Bearer, возвращает t/nil; `%json-ok (data)` — сериализует через cl-json, ставит Content-Type application/json, возвращает строку; `%json-error (msg status)` — аналогично для ошибок.

- [x] **[HTTP-ROUTES]** Добавить в `мастер/cl/http.lisp` роуты через `hunchentoot:define-easy-handler`. Роуты: GET `/api/flows` — проверить auth, вернуть `{"flows": [...]}` из `(активные-потоки)`; POST `/api/dialog` — принять JSON body `{"text":"..."}`, вызвать `(обработать-команду 0 text)`, вернуть `{"response":"..."}` (использовать chat-id=0 для web-сессии); POST `/api/flows/run` — JSON `{"flow":"...","task":"..."}`, вызвать `(запустить-поток flow task)`, вернуть `{"result":"..."}`. Каждый роут проверяет auth через %check-auth, при неудаче возвращает 401.

- [x] **[HTTP-SPAWN]** Добавить в `мастер/cl/http.lisp` роут POST `/api/flows/spawn`. Принять JSON `{"description":"..."}`. Вызвать `(обработать-команду 0 (concatenate 'string "/породить " description))`. Вернуть `{"result":"..."}`.

- [x] **[HTTP-THREAD]** Обновить `мастер/cl/main.lisp`. В функцию `start` добавить в начало: запуск HTTP-сервера через `(bt:make-thread #'start-http-server :name "http-server")`. Добавить импорт bordeaux-threads если нужно.

- [x] **[WEB-INIT]** Создать директорию `web/` в корне проекта. Создать `web/package.json`:
```json
{
  "name": "мастер-pwa",
  "version": "1.0.0",
  "scripts": {
    "dev": "vite",
    "build": "vite build",
    "preview": "vite preview"
  },
  "dependencies": {
    "react": "^18.2.0",
    "react-dom": "^18.2.0"
  },
  "devDependencies": {
    "@vitejs/plugin-react": "^4.0.0",
    "vite": "^5.0.0"
  }
}
```
Создать `web/vite.config.js`:
```js
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
export default defineConfig({ plugins: [react()], base: '/' })
```
Создать `web/index.html` с PWA meta тегами: viewport, apple-mobile-web-app-capable, apple-mobile-web-app-status-bar-style (black-translucent), theme-color (#0a0a0a), manifest link, title "Мастер". Подключить `src/main.jsx`.

- [x] **[WEB-MANIFEST]** Создать `web/public/manifest.json`:
```json
{
  "name": "Мастер",
  "short_name": "Мастер",
  "start_url": "/",
  "display": "standalone",
  "background_color": "#0a0a0a",
  "theme_color": "#0a0a0a",
  "icons": [{"src": "/icon.svg", "sizes": "any", "type": "image/svg+xml"}]
}
```
Создать `web/public/icon.svg` — простая иконка: тёмный фон #141414, буква "М" белым цветом по центру, viewBox="0 0 192 192", размер 192x192.

- [ ] **[WEB-STYLES]** Создать `web/src/styles/global.css` с CSS переменными:
```css
:root {
  --bg: #0a0a0a; --surface: #141414; --surface2: #1e1e1e;
  --border: #2a2a2a; --text: #e8e8e8; --text-dim: #888;
  --accent: #4f8ef7; --accent-dim: #1a2f5a; --error: #e05555;
  --mono: 'SF Mono', 'Menlo', 'Monaco', monospace;
  --radius: 12px; --radius-sm: 8px;
}
* { box-sizing: border-box; margin: 0; padding: 0; }
body { background: var(--bg); color: var(--text); font-family: -apple-system, sans-serif; min-height: 100vh; }
```
Создать `web/src/styles/app.css` со стилями для: `.app` (height: 100dvh, display flex, flex-direction column), `.tab-bar` (position fixed, bottom 0, left/right 0, height 56px + safe-area-inset-bottom, background surface, border-top border), `.tab-btn` (flex 1, кнопка без стилей, иконка + label), `.screen` (flex 1, overflow auto, padding-bottom 56px+safe-area).

- [ ] **[WEB-STORE]** Создать `web/src/store.js`. Экспортировать: `getToken()` — из localStorage 'мастер_token'; `setToken(t)` — сохранить; `clearToken()` — удалить; `getHistory()` — из localStorage 'мастер_history' (JSON массив, default []); `saveHistory(arr)` — сохранить.

- [ ] **[WEB-API]** Создать `web/src/api.js`. Константа `API_BASE` из `import.meta.env.VITE_API_BASE || ''`. Функция `apiFetch(path, options)` — добавляет заголовок `Authorization: Bearer ${getToken()}`, выбрасывает ошибку при 401 (clearToken + reload). Экспортировать: `getFlows()` → GET /api/flows → массив строк; `sendDialog(text)` → POST /api/dialog → строка ответа; `runFlow(flow, task)` → POST /api/flows/run → строка результата; `spawnFlow(description)` → POST /api/flows/spawn → объект {result, flow_name}.

- [ ] **[WEB-AUTH]** Создать `web/src/components/Auth.jsx`. Экран с полем ввода токена и кнопкой "Войти". При сабмите: setToken(value), попробовать getFlows(), если 401 — clearToken + показать ошибку "Неверный токен". Props: `onAuth()` — колбэк при успехе. Стили инлайн или через CSS модуль.

- [ ] **[WEB-TABBAR]** Создать `web/src/components/TabBar.jsx`. Props: `active` (0|1|2), `onChange(i)`. Три таба: 💬 Диалог / ⚡ Потоки / ✨ Породить. Активный таб — цвет accent. Fixed внизу с padding-bottom env(safe-area-inset-bottom).

- [ ] **[WEB-CHAT]** Создать `web/src/components/Chat.jsx`. Хранит историю в localStorage через store.js. Показывает пузыри сообщений: своё — справа, тёмно-синий фон; ответ мастера — слева, surface фон, моноширинный шрифт. Input внизу + кнопка отправить. При отправке: добавить в историю {role:'user', text}, вызвать sendDialog(text), добавить {role:'assistant', text: result}. Пока ожидание — показывать пузырь "●●●" (анимация). useRef для скролла вниз после каждого сообщения.

- [ ] **[WEB-FLOWS]** Создать `web/src/components/Flows.jsx`. При маунте загрузить getFlows(). Список потоков — карточки. Тап на карточку → показать внутри неё поле задачи + кнопку Запустить (expand/collapse через useState). После runFlow — показать результат под полем, моноширинный шрифт, скролл к результату. Loading state на кнопке.

- [ ] **[WEB-SPAWN]** Создать `web/src/components/Spawn.jsx`. Большой textarea "Опиши новый поток на русском...". Кнопка "Породить ✨". При сабмите: вызвать spawnFlow(description). После успеха: зелёный баннер с именем потока + кнопка "Попробовать" (переключает на вкладку Потоки). При ошибке — красный баннер.

- [ ] **[WEB-APP]** Создать `web/src/App.jsx`. useState для activeTab (0,1,2) и isAuthed (!!getToken()). Если !isAuthed — рендерить Auth с onAuth колбэком. Иначе: рендерить текущий экран (Chat/Flows/Spawn) + TabBar внизу. Создать `web/src/main.jsx`: импорт React, ReactDOM, App, global.css, app.css; ReactDOM.createRoot(document.getElementById('root')).render(<App/>).

- [ ] **[WEB-NGINX]** Создать `web/nginx.conf`:
```nginx
server {
    listen 80;
    root /path/to/web/dist;
    index index.html;
    location /api/ {
        proxy_pass http://localhost:7070;
        proxy_set_header Authorization $http_authorization;
    }
    location / {
        try_files $uri $uri/ /index.html;
    }
}
```

- [ ] **[VERIFY-CL]** Проверить что мастер/cl/мастер.asd и http.lisp синтаксически корректны: запустить `sbcl --noinform --eval "(load \"мастер/cl/packages.lisp\")" --eval "(sb-ext:exit)" 2>&1`. Если errors — исправить.

- [ ] **[VERIFY-WEB]** В директории web/ запустить `npm install && npm run build`. Если ошибки — исправить. Проверить что в dist/ есть index.html.
