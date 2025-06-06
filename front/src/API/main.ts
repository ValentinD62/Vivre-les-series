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

export async function getRatingSerieBySerieAndUserId(serieId: number, userId: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Series/rating?serieId=${serieId}&userId=${userId}`, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json',
        'Authorization': `Bearer ${localStorage.getItem("token")}` },
    });
    const response = await fetch(request);
    if (response.status === 404) {
        return null; // Pas de note trouvée
    }
    return await response.json();
}

export async function postRatingSerieBySerieAndUserId(serieId: number, userId: number, rating: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Series/${serieId}/rating?userId=${userId}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json',
            'Authorization': `Bearer ${localStorage.getItem("token")}` },
        body: JSON.stringify({ "value": rating }),
    });
    const response = await fetch(request);
    const data = await response.json();
    if (response.status !== 204 && response.status !== 201) { //Created
        throw new Error(data.message || `HTTP error! status: ${response.status}`);
    }
    return data;
}

export async function deleteRatingSerieBySerieAndUserId(serieId: number, userId: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Series/rating?serieId=${serieId}&userId=${userId}`, {
        method: 'DELETE',
        headers: { 'Content-Type': 'application/json',
            'Authorization': `Bearer ${localStorage.getItem("token")}` },
    });
    const response = await fetch(request);
    return response.ok;
}

export async function getAllCommentsBySerieId(serieId: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/comments/serie/${serieId}`, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json',
            'Authorization': `Bearer ${localStorage.getItem("token")}` },
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}

export async function getCommentsBySerieIdAndUserId(serieId: number, userId: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/comments/serie/${serieId}/${userId}`, {
        method: 'GET',
        headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${localStorage.getItem("token")}`
        },
    });
    const response = await fetch(request);
    if (response.status === 204) {
        return null; //Pas de commentaire trouvé
    }
    return await response.json();
}

export async function postComments(serieId: number, userId: number, comment: string): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/comments`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${localStorage.getItem("token")}`
        },
        body: JSON.stringify({ content: comment , serieId : serieId, userId : userId }),
    });
    const response = await fetch(request);
    if (response.status !== 201) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}

export async function putComments(commentId: number, content: string, createdAt: string, userId: number, serieId: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/comments/${commentId}`, {
        method: 'PUT',
        headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${localStorage.getItem("token")}`
        },
        body: JSON.stringify({ id: commentId, content: content, createdAt: createdAt, serieId: serieId, userId: userId }),
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}

export async function deleteComments(commentId: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/comments/${commentId}`, {
        method: 'DELETE',
        headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${localStorage.getItem("token")}`
        },
    });
    const response = await fetch(request);
    if (response.status !== 204) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return true;
}

export async function getUserById(userId: number): Promise<any> {
    const request: RequestInfo = new Request(`${API_BASE_URL}/Users/search/${userId}`, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json',
            'Authorization': `Bearer ${localStorage.getItem("token")}` },
    });
    const response = await fetch(request);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
}