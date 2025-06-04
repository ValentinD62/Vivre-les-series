export function checkImage(url: string): Promise<boolean> {
    return new Promise((resolve) => {
        const img = new Image();
        img.src = url;

        img.onload = () => resolve(true);  // Image chargée avec succès
        img.onerror = () => resolve(false); // Erreur lors du chargement
    });
}
export function debounce<T extends (...args: any[]) => void>(func: T, wait: number): (...args: Parameters<T>) => void {
    let timeout: number | undefined;
    return (...args: Parameters<T>) => {
        clearTimeout(timeout);
        timeout = window.setTimeout(() => func(...args), wait);
    };
}

function isValidUsername(username: string): string {
    if (/^[a-zA-Z0-9_]{3,20}$/.test(username)) {
        return "";
    } else {
        return "Le pseudo doit contenir entre 3 et 20 caractères, et ne peut contenir que des lettres, des chiffres et des tirets bas.";
    }
}

function isValidPassword(password: string): string {
    return (password.length >= 4 && password.length <= 20) ? "" : "Le mot de passe doit contenir entre 4 et 20 caractères.";
}

export function escapeHtml(input: string): string {
    return input.replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}

export function validateUserInput(username: string, password: string): string {
    const validUsername = isValidUsername(username);
    const validPassword = isValidPassword(password);
    let allMessage = "";
    if (validUsername !== "") {
        allMessage = allMessage.concat(validUsername);
    }
    if (validPassword !== "") {
        allMessage = allMessage.concat(validPassword);
    }
    return allMessage;
}