using BetCo.Entity;

namespace BetCo.Core.Business
{
    public interface IBetService
    {
        IEnumerable<Bet> RetrieveAll();
        Bet RetrieveById(int id);

        Bet Add(Bet bet);
        Bet Update(int id, Bet bet);
        bool Remove(int id);
    }
}