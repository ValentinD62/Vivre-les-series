using System.Net.Http.Json;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Entity;
using System.Text.Json;
using System.Text;
using System.Net.Http.Headers;
using System.Net;
using Microsoft.EntityFrameworkCore;
using VivreLesSeries.Repository.Context;

namespace VivreLesSeries.Repository
{
    public class SerieRepository : ISerieRepository
    {

        private readonly UserSerieContext _context;
        private readonly HttpClient _httpClient;
        private const string BearerToken = "eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiJmNDAxNTBlZjMwNjYzNjZhZmJkYTNiMzk0ZmE0MzBhMCIsIm5iZiI6MTc0NzM5OTE2My44MTc5OTk4LCJzdWIiOiI2ODI3MzFmYjNlZWVjODgyNzRhZGE2ZmYiLCJzY29wZXMiOlsiYXBpX3JlYWQiXSwidmVyc2lvbiI6MX0.m2uo_7GXv2k5id-mRw9d66ocaOq3A_rufbdMwveRSi0";

        public SerieRepository(UserSerieContext context)
        {
            _httpClient = new HttpClient();
            //Obligatoire pour appeler l'API
            _httpClient.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", BearerToken);
            _context = context;
        }

        public async Task<List<Serie>> GetTopRatedSeriesAsync()
        {
            var url = $"https://api.themoviedb.org/3/tv/top_rated?language=fr-FR&page=1";
            var response = await _httpClient.GetFromJsonAsync<TmdbResponse>(url);
            if (response?.Results != null)
            {
                for (int i = 0; i < response.Results.Count; i++)
                {
                    response.Results[i].ImagePath = $"https://image.tmdb.org/t/p/w1920{response.Results[i].ImagePath}";
                }
            }
            return response?.Results ?? new List<Serie>();
        }
       
        public async Task<List<Serie>> GetSeriesByNameAsync(string name)
        {
            var url = $"https://api.themoviedb.org/3/search/tv?query={name}&include_adult=true&language=en-US&page=1";
            var response = await _httpClient.GetFromJsonAsync<TmdbResponse>(url);
            if (response?.Results != null)
            {
                for (int i = 0; i < response.Results.Count; i++)
                {
                    response.Results[i].ImagePath = $"https://image.tmdb.org/t/p/w1920{response.Results[i].ImagePath}";
                }
            }
            return response?.Results ?? new List<Serie>();
        }

        public async Task<Serie> GetSerieByIdAsync(int id)
        {
            var url = $"https://api.themoviedb.org/3/tv/{id}";
            var response = await _httpClient.GetFromJsonAsync<Serie>(url);
            if (response != null)
            {
                response.ImagePath = $"https://image.tmdb.org/t/p/w1920{response.ImagePath}";
            }
            return response ?? new Serie();
        }

        public async Task<string> GetImagePathByIdAsync(int serieId)
        {
            var url = $"https://api.themoviedb.org/3/tv/{serieId}/images";
            var response = await _httpClient.GetFromJsonAsync<TmdbResponse>(url);
            var path = "";
            if (response?.ImagesResults != null)
            {
                var mainImage = response.ImagesResults[0];
                path = $"https://image.tmdb.org/t/p/w{mainImage.Width}{mainImage.FilePath}";
            }
            return path;
        }
        public async Task<Rating?> GetRatingByUserAndSerieAsync(int userId, int serieId)
        {
            return await _context.Rating
                .FirstOrDefaultAsync(r => r.UserId == userId && r.SerieId == serieId);
        }

        public async Task<HttpStatusCode> AddRatingAsync(int serieId, Rating rating)
        {
            var url = $"https://api.themoviedb.org/3/tv/{serieId}/rating";

            var payload = new { value = rating.Value};
            var json = JsonSerializer.Serialize(payload);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            var response = await _httpClient.PostAsync(url, content);

            // Vérifie s'il y a déjà une note pour cet utilisateur et cette série
            var ratingExists = await _context.Rating
                .AnyAsync(r => r.UserId == rating.UserId && r.SerieId == rating.SerieId);

            if (ratingExists)
            {
                response.StatusCode = HttpStatusCode.Forbidden;
            } else
            {
                //On l'ajoute également à la base de données
                _context.Rating.Add(rating);
                await _context.SaveChangesAsync();
            }
            return response.StatusCode;
        }

        public async Task<HttpStatusCode> DeleteRatingAsync(int serieId, int userId)
        {
            var url = $"https://api.themoviedb.org/3/tv/{serieId}/rating";
            var request = new HttpRequestMessage(HttpMethod.Delete, url);
            var response = await _httpClient.SendAsync(request);

            var rating = await _context.Rating.FirstOrDefaultAsync(r => r.UserId == userId && r.SerieId == serieId);

            if (rating == null)
                return HttpStatusCode.NotFound;

            _context.Rating.Remove(rating);
            await _context.SaveChangesAsync();

            return response.StatusCode;
        }
    }
}