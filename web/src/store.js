// localStorage utilities для мастер PWA

const TOKEN_KEY = 'мастер_token';
const HISTORY_KEY = 'мастер_history';

export function getToken() {
  return localStorage.getItem(TOKEN_KEY);
}

export function setToken(token) {
  localStorage.setItem(TOKEN_KEY, token);
}

export function clearToken() {
  localStorage.removeItem(TOKEN_KEY);
}

export function getHistory() {
  try {
    const data = localStorage.getItem(HISTORY_KEY);
    return data ? JSON.parse(data) : [];
  } catch (e) {
    console.error('Failed to parse history:', e);
    return [];
  }
}

export function saveHistory(arr) {
  localStorage.setItem(HISTORY_KEY, JSON.stringify(arr));
}
