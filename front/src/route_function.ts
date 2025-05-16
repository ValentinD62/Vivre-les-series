export function getHeaders(): Headers {
  const headers: Headers = new Headers()
  // Add a few headers
  headers.set('Content-Type', 'application/json')
  return headers;
}
