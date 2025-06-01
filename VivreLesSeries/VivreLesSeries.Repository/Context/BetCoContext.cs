using BetCo.Entity;
using Microsoft.EntityFrameworkCore;

namespace BetCo.Repository.Context
{
    public class BetCoContext : DbContext
    {

        public BetCoContext(DbContextOptions<BetCoContext> options)
            :base(options)
        {
            
        }

        public DbSet<Bet> Bets { get; set; }
        public DbSet<User> Users { get; set; }
    }
}
