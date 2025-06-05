using Microsoft.EntityFrameworkCore;
using VivreLesSeries.Entity;

namespace VivreLesSeries.Repository.Context
{
    public class UserSerieContext : DbContext
    {
        public UserSerieContext(DbContextOptions<UserSerieContext> options)
            : base(options) 
        {

        }

        public DbSet<User> Users { get; set; }
        public DbSet<UserSession> UserSessions { get; set; }
        public DbSet<Comment> Comments { get; set; }
        public DbSet<Rating> Rating { get; set; }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);
        }
    }
}