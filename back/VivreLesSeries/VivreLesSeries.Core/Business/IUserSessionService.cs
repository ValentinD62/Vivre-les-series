using VivreLesSeries.Entity;

namespace VivreLesSeries.Core.Business
{
    public interface IUserSessionService
    {
        public Task<UserSession> LinkTmdbSessionAsync(int userId, string requestToken);
    }
}
