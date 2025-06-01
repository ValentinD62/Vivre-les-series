using System.ComponentModel.DataAnnotations;

namespace BetCo.Entity
{
    public class User
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public string Email { get; set; }
        public DateTime CreationDate{ get; set; }
        public IEnumerable<Bet> Bets { get; set; }
    }
}
