namespace VivreLesSeries.Entity
{
    public class Rating
    {
        public int Id { get; set; }
        public double Value{ get; set; }
        public DateTime CreatedAt { get; set; }

        public int UserId { get; set; }

        public int SerieId { get; set; }
    }
}
