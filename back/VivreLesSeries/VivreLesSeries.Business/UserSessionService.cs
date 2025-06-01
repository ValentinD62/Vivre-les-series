using VivreLesSeries.Core.Business;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Entity;

namespace VivreLesSeries.Business
{
    public class UserSessionService : IUserSessionService
    {
        private readonly IUserRepository _userRepository;
        private readonly TmdbService _tmdbService;

        public UserSessionService(IUserRepository userRepository, TmdbService tmdbService)
        {
            _userRepository = userRepository;
            _tmdbService = tmdbService;
        }

        public async Task<UserSession> LinkTmdbSessionAsync(int userId, string requestToken)
        {
            var tmdbSessionId = await _tmdbService.CreateTmdbSessionAsync(requestToken);

            var session = new UserSession
            {
                UserId = userId,
                TmdbSessionId = tmdbSessionId,
                CreatedAt = DateTime.UtcNow
            };

            await _userRepository.AddSessionAsync(session);
            return session;
        }
    }

}
