const API_BASE_URL = "https://localhost:7094/api";

export async function getAllSeries(): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Series/list`, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json' },
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}

export async function getSeriesByName(name: string): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Series/searchbyname/${name}`, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json' },
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}

export async function getSeriesById(id: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Series/searchbyid/${id}`, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json' },
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}

export async function postCreateUser(name: string, password: string): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Users/createuser`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ name, password }),
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}

export async function postLoginUser(name: string, password: string): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Users/login`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ name, password }),
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}

export async function postRateSerie(serieId: number, userId: number, rating: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Series/${serieId}/rating`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ userId, rating }),
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}