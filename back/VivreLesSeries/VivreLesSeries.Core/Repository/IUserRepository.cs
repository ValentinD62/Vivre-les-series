using VivreLesSeries.Entity;

namespace VivreLesSeries.Core.Repository
{
    public interface IUserRepository
    {
        Task<User> LoginAsync(string name, string password);
        Task<User> CreateUserAsync(User user);
        Task<UserSession> AddSessionAsync(int userId, string tmdbSessionId);
        Task<User> GetUserByIdAsync(int id);
        Task<UserSession> AddSessionAsync(UserSession session);

    }
}