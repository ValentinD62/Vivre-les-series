using BetCo.Entity;

namespace BetCo.Core.Repository
{
    public interface IBetRepository
    {
        bool Delete(int id);
        Bet Insert(Bet bet);
        public IEnumerable<Bet> SelectAll();
        Bet SelectById(int id);
        Bet Update(int id, Bet bet);
    }
}
