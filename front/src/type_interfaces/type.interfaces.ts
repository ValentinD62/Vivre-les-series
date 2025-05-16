export interface User {
    userId: number;
    userFirstName: string;
    userLastName: string;
    userPassword: string;
    userPicture: string;
}

export interface Series {
    seriesId: number;
    seriesTitle: string;
    seriesDescription: string;
    seriesPicture: string;
    seriesReleaseDate: string;
    seriesDuration: number;
    seriesRating: number;
}

export interface Favorites {
    favoriteId: number;
    userId: number;
    seriesId: number;
}

export interface Comments {
    commentId: number;
    userId: number;
    seriesId: number;
    commentTitle: string;
    commentBody: string;
    commentNote: number;
}
