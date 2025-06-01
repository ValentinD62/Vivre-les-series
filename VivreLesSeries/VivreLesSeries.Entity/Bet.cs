using System.ComponentModel.DataAnnotations;

namespace BetCo.Entity
{
    public class Bet
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
        public DateTime Date { get; set; }
        public string CompetitorA { get; set; }
        public string CompetitorB { get; set; }
        public int UserId { get; set; }
    }
}
