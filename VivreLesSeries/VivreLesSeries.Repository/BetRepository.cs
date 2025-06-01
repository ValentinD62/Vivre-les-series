using BetCo.Core.Repository;
using BetCo.Entity;
using BetCo.Repository.Context;
namespace BetCo.Repository
{
    public class BetRepository : IBetRepository
    {
        private readonly BetCoContext _dbContext;

        public BetRepository(BetCoContext dbContext)
        {
            _dbContext = dbContext;   
        }

        public bool Delete(int id)
        {
            var betToDelete = _dbContext.Bets.FirstOrDefault(bets => bets.Id == id);

            _dbContext.Bets.Remove(betToDelete);

            _dbContext.SaveChanges();

            return true;
        }

        public Bet Insert(Bet bet)
        {
            _dbContext.Bets.Add(bet);

            _dbContext.SaveChanges();

            return bet;
        }

        public IEnumerable<Bet> SelectAll()
        {
            return _dbContext.Bets;
        }

        public Bet SelectById(int id)
        {
            return _dbContext.Bets.FirstOrDefault(bet => bet.Id == id);
        }

        public Bet Update(int id, Bet bet)
        {
            var betToUpdate = _dbContext.Bets.First(bet => bet.Id == id);

            betToUpdate.Id = bet.Id;
            betToUpdate.Name = bet.Name;
            betToUpdate.Date = bet.Date;
            betToUpdate.Description = bet.Description;
            betToUpdate.CompetitorA = bet.CompetitorA;
            betToUpdate.CompetitorB = bet.CompetitorB;

            _dbContext.SaveChanges();

            return betToUpdate;
        }
    }
}

























//return new List<Bet>()
//{
//    new Bet
//    {
//        Id = 1,
//        Description = "description",
//        Date = DateTime.Now,
//        Name = "name",
//        CompetitorA = "Jérémy C",
//        CompetitorB = "Pacco"
//    },
//    new Bet
//    {
//        Id = 2,
//        Description = "description 2",
//        Date = DateTime.Now,
//        Name = "name2",
//        CompetitorA = "POPIEUL",
//        CompetitorB = "Goudal"
//    },
//};