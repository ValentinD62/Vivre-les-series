const API_BASE_URL = "https://localhost:7094/api";

export async function getAllSeries(): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Series/list`, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json',
        },
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}