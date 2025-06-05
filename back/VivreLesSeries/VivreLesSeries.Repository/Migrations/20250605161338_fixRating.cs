using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace VivreLesSeries.Repository.Migrations
{
    /// <inheritdoc />
    public partial class fixRating : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropForeignKey(
                name: "FK_Rating_Serie_SerieId",
                table: "Rating");

            migrationBuilder.DropForeignKey(
                name: "FK_Rating_Users_UserId",
                table: "Rating");

            migrationBuilder.DropIndex(
                name: "IX_Rating_SerieId",
                table: "Rating");

            migrationBuilder.DropIndex(
                name: "IX_Rating_UserId",
                table: "Rating");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateIndex(
                name: "IX_Rating_SerieId",
                table: "Rating",
                column: "SerieId");

            migrationBuilder.CreateIndex(
                name: "IX_Rating_UserId",
                table: "Rating",
                column: "UserId");

            migrationBuilder.AddForeignKey(
                name: "FK_Rating_Serie_SerieId",
                table: "Rating",
                column: "SerieId",
                principalTable: "Serie",
                principalColumn: "Id",
                onDelete: ReferentialAction.Cascade);

            migrationBuilder.AddForeignKey(
                name: "FK_Rating_Users_UserId",
                table: "Rating",
                column: "UserId",
                principalTable: "Users",
                principalColumn: "Id",
                onDelete: ReferentialAction.Cascade);
        }
    }
}
