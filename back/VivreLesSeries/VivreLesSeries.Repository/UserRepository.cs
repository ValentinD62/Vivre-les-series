﻿using Microsoft.EntityFrameworkCore;
using VivreLesSeries.Entity;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Repository.Context;

namespace VivreLesSeries.Repository
{
    public class UserRepository : IUserRepository
    {
        private readonly UserSerieContext _context;

        public UserRepository(UserSerieContext context)
        {
            _context = context;
        }

        public async Task<User?> GetUserById(int id)
        {
            var searchUser = await _context.Users.FindAsync(id);
            return searchUser;
        }

        public async Task<User> LoginAsync(string name, string password)
        {
            var searchUser = await _context.Users.FirstOrDefaultAsync(u => u.Name == name && u.Password == password);
            return searchUser ?? new User();
        }

        public async Task<User> CreateUserAsync(User user)
        {
            _context.Users.Add(user);
            await _context.SaveChangesAsync();
            return user;
        }

        public async Task<UserSession> AddSessionAsync(int userId, string tmdbSessionId)
        {
            var session = new UserSession
            {
                UserId = userId,
                TmdbSessionId = tmdbSessionId,
                CreatedAt = DateTime.UtcNow
            };
            _context.UserSessions.Add(session);
            await _context.SaveChangesAsync();
            return session;
        }

        public async Task<User> GetUserByIdAsync(int id)
        {
            return await _context.Users
                .Include(u => u.Sessions)
                .FirstOrDefaultAsync(u => u.Id == id);
        }

        public async Task<UserSession> AddSessionAsync(UserSession session)
        {
            _context.UserSessions.Add(session);
            await _context.SaveChangesAsync();
            return session;
        }
    }
}
