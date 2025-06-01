using VivreLesSeries.Entity;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Core.Business;
using System.Net;

namespace VivreLesSeries.Business
{
    public class SerieService : ISeriesService
    {
        private readonly ISerieRepository _repository;

        public SerieService(ISerieRepository repository)
        {
            _repository = repository;
        }

        public async Task<List<Serie>> GetTopRatedSeriesAsync()
        {
            return await _repository.GetTopRatedSeriesAsync();
        }

        public async Task<List<Serie>> GetSeriesByNameAsync(string name)
        {
            return await _repository.GetSeriesByNameAsync(name);
        }

        public async Task<Serie> GetSerieByIdAsync(int id)
        {
            return await _repository.GetSerieByIdAsync(id);
        }

        public async Task<string> GetImagePathByIdAsync(int serieId)
        {
            var listPath = await _repository.GetImagePathByIdAsync(serieId);
            return listPath;
        }

        public async Task<HttpStatusCode> AddRatingAsync(int serieId, string sessionId, double ratingValue)
        {
            return await _repository.AddRatingAsync(serieId, sessionId, ratingValue);
        }

        public async Task<HttpStatusCode> DeleteRatingAsync(int serieId, string sessionId)
        {
            return await _repository.DeleteRatingAsync(serieId, sessionId);
        }
    }
}