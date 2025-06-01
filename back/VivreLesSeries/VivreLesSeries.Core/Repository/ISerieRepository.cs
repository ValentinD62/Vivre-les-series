using System.Net;
using VivreLesSeries.Entity;

namespace VivreLesSeries.Core.Repository
{
    public interface ISerieRepository
    {
        Task<List<Serie>> GetTopRatedSeriesAsync();
        Task<List<Serie>> GetSeriesByNameAsync(string name);
        Task<Serie> GetSerieByIdAsync(int id);
        Task<string> GetImagePathByIdAsync(int id);
        Task<HttpStatusCode> AddRatingAsync(int serieId, string sessionId, double ratingValue);
        Task<HttpStatusCode> DeleteRatingAsync(int serieId, string sessionId);
    }
}
