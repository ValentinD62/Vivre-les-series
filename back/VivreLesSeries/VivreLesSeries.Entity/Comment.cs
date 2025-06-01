namespace VivreLesSeries.Entity
{
    public class Comment
    {
        public int Id { get; set; }
        public string Content { get; set; }
        public DateTime CreatedAt { get; set; }

        // Lien avec l'utilisateur
        public int UserId { get; set; }
        public User User { get; set; }

        // Lien avec la série
        public int SerieId { get; set; }
        public Serie Serie { get; set; }
    }

}
