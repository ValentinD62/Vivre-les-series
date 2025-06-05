namespace VivreLesSeries.Entity
{
    public class Rating
    {
        public int Id { get; set; }
        public double Value{ get; set; }
        public DateTime CreatedAt { get; set; }

        // Lien avec l'utilisateur
        public int UserId { get; set; }
        public User User { get; set; }

        // Lien avec la série
        public int SerieId { get; set; }
        public Serie Serie { get; set; }
    }
}
