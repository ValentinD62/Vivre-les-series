using System.Text.Json.Serialization;

namespace VivreLesSeries.Entity
{
    public class Image
    {
        [JsonPropertyName("file_path")]
        public required string FilePath { get; set; }

        [JsonPropertyName("width")]
        public required int Width { get; set; }

        [JsonPropertyName("height")]
        public int? Height { get; set; }
    }
}
