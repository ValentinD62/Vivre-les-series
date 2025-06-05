using System.Net;
using VivreLesSeries.Entity;
using VivreLesSeries.Entity.DTO;

namespace VivreLesSeries.Core.Repository
{
    public interface ISerieRepository
    {
        Task<List<Serie>> GetTopRatedSeriesAsync();
        Task<List<Serie>> GetSeriesByNameAsync(string name);
        Task<Serie> GetSerieByIdAsync(int id);
        Task<string> GetImagePathByIdAsync(int id);
        Task<HttpStatusCode> AddRatingAsync(int serieId, Rating rating);
        Task<HttpStatusCode> DeleteRatingAsync(int serieId);
    }
}
