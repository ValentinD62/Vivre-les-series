using System.Text;
using System.Text.Json;
using VivreLesSeries.Core.Business;

namespace VivreLesSeries.Business
{
    public class TmdbService : ITmdbService
    {
        private readonly HttpClient _httpClient;
        private readonly string _apiKey;

        public TmdbService(HttpClient httpClient, string apiKey)
        {
            _httpClient = httpClient;
            _apiKey = apiKey;
        }

        public async Task<string> CreateTmdbSessionAsync(string requestToken)
        {
            var url = $"https://api.themoviedb.org/3/authentication/session/new?api_key={_apiKey}";

            var content = new StringContent(JsonSerializer.Serialize(new { request_token = requestToken }), Encoding.UTF8, "application/json");
            var response = await _httpClient.PostAsync(url, content);

            if (!response.IsSuccessStatusCode)
            {
                throw new Exception($"Erreur TMDB: {response.StatusCode}");
            }

            var json = await response.Content.ReadAsStringAsync();
            using var doc = JsonDocument.Parse(json);
            return doc.RootElement.GetProperty("session_id").GetString();
        }
    }

}
