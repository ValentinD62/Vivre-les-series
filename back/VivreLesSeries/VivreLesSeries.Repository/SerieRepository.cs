using System.Net.Http.Json;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Entity;
using System.Text.Json;
using System.Text;
using System.Net.Http.Headers;
using System.Net;

namespace VivreLesSeries.Repository
{
    public class SerieRepository : ISerieRepository
    {
        private readonly HttpClient _httpClient;
        private const string BearerToken = "eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiJmNDAxNTBlZjMwNjYzNjZhZmJkYTNiMzk0ZmE0MzBhMCIsIm5iZiI6MTc0NzM5OTE2My44MTc5OTk4LCJzdWIiOiI2ODI3MzFmYjNlZWVjODgyNzRhZGE2ZmYiLCJzY29wZXMiOlsiYXBpX3JlYWQiXSwidmVyc2lvbiI6MX0.m2uo_7GXv2k5id-mRw9d66ocaOq3A_rufbdMwveRSi0";

        public SerieRepository()
        {
            _httpClient = new HttpClient();
            //Obligatoire pour appeler l'API
            _httpClient.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", BearerToken);
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

        public async Task<HttpStatusCode> AddRatingAsync(int serieId, string sessionId, double ratingValue)
        {
            var url = $"https://api.themoviedb.org/3/tv/{serieId}/rating?session_id={sessionId}";

            var payload = new { value = ratingValue };
            var json = JsonSerializer.Serialize(payload);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            var response = await _httpClient.PostAsync(url, content);
            return response.StatusCode;
        }

        public async Task<HttpStatusCode> DeleteRatingAsync(int serieId, string sessionId)
        {
            var url = $"https://api.themoviedb.org/3/tv/{serieId}/rating?session_id={sessionId}";
            var request = new HttpRequestMessage(HttpMethod.Delete, url);
            var response = await _httpClient.SendAsync(request);

            return response.StatusCode;
        }
    }
}