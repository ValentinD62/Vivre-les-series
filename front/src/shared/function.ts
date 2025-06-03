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