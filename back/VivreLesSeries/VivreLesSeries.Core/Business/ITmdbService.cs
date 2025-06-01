namespace VivreLesSeries.Core.Business
{
    public interface ITmdbService
    {
        public Task<string> CreateTmdbSessionAsync(string requestToken);
    }

}
