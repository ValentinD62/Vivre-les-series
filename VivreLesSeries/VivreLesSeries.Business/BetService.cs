using BetCo.Core.Business;
using BetCo.Core.Repository;
using BetCo.Entity;

namespace BetCo.Business
{
    public class BetService : IBetService
    {
        private readonly IBetRepository _betRepository;

        public BetService(IBetRepository betRepository)
        {
            _betRepository = betRepository;
        }

        public Bet Add(Bet bet)
        {
            return _betRepository.Insert(bet);
        }

        public bool Remove(int id)
        {
            return _betRepository.Delete(id);
        }

        public IEnumerable<Bet> RetrieveAll()
        {
            return _betRepository.SelectAll();
        }

        public Bet RetrieveById(int id)
        {
            return _betRepository.SelectById(id);
        }

        public Bet Update(int id, Bet bet)
        {
            return _betRepository.Update(id, bet);
        }
    }
}