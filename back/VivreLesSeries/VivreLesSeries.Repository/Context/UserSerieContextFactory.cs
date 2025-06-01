using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Design;

namespace VivreLesSeries.Repository.Context
{
    public class UserSerieContextFactory : IDesignTimeDbContextFactory<UserSerieContext>
    {
        public UserSerieContext CreateDbContext(string[] args)
        {
            var optionsBuilder = new DbContextOptionsBuilder<UserSerieContext>();
            var databasePath = Path.Combine(Directory.GetCurrentDirectory(), "Database", "VivreLesSeries.db");
            optionsBuilder.UseSqlite($"Data Source={databasePath}");

            return new UserSerieContext(optionsBuilder.Options);
        }
    }
}
