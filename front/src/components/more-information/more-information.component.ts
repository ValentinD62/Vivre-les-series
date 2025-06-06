import {customElement, property, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import MoreInformationCSS from "./more-information.scss?inline";
import {checkImage} from "../../shared/function.ts";
import {
  deleteComments,
  deleteRatingSerieBySerieAndUserId,
  getAllCommentsBySerieId,
  getRatingSerieBySerieAndUserId,
  getUserById, postComments,
  postRatingSerieBySerieAndUserId, putComments
} from "../../API/main.ts";

//Composant pour afficher les informations d'une série au clic sur l'une d'entre elles
@customElement('more-information-component')
export class MoreInformationComponent extends LitElement {

  @property({ type: Number, attribute: false })
  series: { id: number, name: string; vote_average: number; first_air_date: string; overview: string; backdrop_path: string } = {
    name: "",
    vote_average: 0,
    id: 0,
    first_air_date: "",
    overview: "",
    backdrop_path: ""
  };

  @state()
  loading: boolean = true;

  @state()
  userNote: number = 0;

  @state()
  errorRating: string = "";

  @state()
  commentsWithUsers: Array<{ id: number, userId: number, createdAt: string, serieId: number, content: string, userName: string }> = [];

  @state()
  hasAlreadyCommented: boolean = false;

  override connectedCallback(): void {
    super.connectedCallback();
    document.addEventListener("mousedown", this.handleOutsideClick);
  }

  override disconnectedCallback(): void {
    super.disconnectedCallback();
    document.removeEventListener("mousedown", this.handleOutsideClick);
  }

  override firstUpdated() {
    this.getUserNote();
    this.getAllComments();
  }

  private handleOutsideClick = (event: MouseEvent): void => {
    const path = event.composedPath();
    if (!path.includes(this)) {
      this.dispatchEvent(new CustomEvent('notDisplayMoreInformation'));
    }
  };

  private getUserNote() {
    const userId = localStorage.getItem("userId");
    if (userId) {
      try {
        getRatingSerieBySerieAndUserId(this.series.id, parseInt(userId))
            .then( (rating) => {
              this.userNote = rating.value;
            })
            .catch((error) => {
              console.error("Erreur lors de la récupération de la note de l'utilisateur :", error);
              this.userNote = 0;
            });
      } catch (error) {
        console.error("Erreur lors de la récupération de la note de l'utilisateur :", error);
        return html`0`;
      }
    }
    return html`0`;
  }

  private deleteRating() {
    deleteRatingSerieBySerieAndUserId(this.series.id, parseInt(<string>localStorage.getItem("userId")))
        .then(() => {
            this.getUserNote(); })
        .catch((error) => {
            console.error("Erreur lors de la suppression de la note :", error);
            this.errorRating = "Erreur lors de la suppression de la note";
        });
  }

  private submitRating(event: Event) {
    event.preventDefault();
    postRatingSerieBySerieAndUserId(this.series.id,
        parseInt(<string>localStorage.getItem("userId")),
        parseFloat((event.target as HTMLFormElement).rating.value)
    ).then(() => {
      this.getUserNote();
    })
        .catch((error) => {
          console.error("Erreur que je ne comprends pas :", error);
          this.getUserNote();
    });
  }

  private async getAllComments() {
    try {
      const comments = await getAllCommentsBySerieId(this.series.id);
      this.commentsWithUsers = await Promise.all(
          comments.map(async (comment: any) => {
            try {
              if(comment.userId === parseInt(<string>localStorage.getItem("userId"))) {
                this.hasAlreadyCommented = true;
                return {
                  ...comment,
                  userName: "Vous"
                }
              }
              const user = await getUserById(comment.userId);
              return {
                ...comment,
                userName: user?.name ?? `Utilisateur ${comment.userId}`
              };
            } catch {
              return {
                ...comment,
                userName: `Utilisateur ${comment.userId}`
              };
            }
          })
      );
    } catch (error) {
      console.error("Erreur lors de la récupération des commentaires :", error);
    }
  }

  private renderAllComments() {
    return this.commentsWithUsers.map((comment) => html`
    <div class="${comment.userId === parseInt(<string>localStorage.getItem("userId")) ? "more-information-container__user-comment" : "more-information-container__comment" }">
      <div class="more-information-container__comment-user"> 
        De: ${comment.userName}
      </div>
      <div class="more-information-container__comment-content">
        ${comment.content}
        <div>
          ${comment.userId === parseInt(<string>localStorage.getItem("userId")) ? html`<img src="corbeille.png" alt="delete comment" @click="${this.deleteUserComment}">` : html``}
          ${comment.userId === parseInt(<string>localStorage.getItem("userId")) ? html`<img src="pencil.png" alt="modify comment" @click="${this.modifyUserComment}">` : html``}
        </div>
      </div>
    </div>
  `);
  }

  private async submitComment(event: Event) {
    event.preventDefault();
    const form = event.target as HTMLFormElement;
    const comment = form.comment.value.trim();
    const userId = localStorage.getItem("userId");
    if (!comment || !userId) return;

    try {
      await postComments(this.series.id, parseInt(userId), comment);
      form.reset();
      await this.getAllComments();
    } catch (error) {
      console.error("Erreur lors de l'envoi du commentaire :", error);
    }
  }

  private deleteUserComment() {
    const userId = parseInt(<string>localStorage.getItem("userId"));
    const userComment = this.commentsWithUsers.find(c => c.userId === userId);
    if (!userComment) return;
    try {
      deleteComments(userComment.id).then(() => {
        this.getAllComments().then(() => {});
      });
      this.hasAlreadyCommented = false;
    } catch (error) {
      console.error("Erreur lors de la suppression du commentaire :", error);
    }
  }

  private async modifyUserComment() {
    const userId = parseInt(<string>localStorage.getItem("userId"));
    const userComment = this.commentsWithUsers.find(c => c.userId === userId);
    if (!userComment) return;
    const newContent = prompt("Modifiez votre commentaire :", userComment.content);
    if (newContent === null || newContent.trim() === "" || newContent === userComment.content) return;
    try {
      putComments(
          userComment.id,
          newContent.trim(),
          userComment.createdAt,
          userId,
          this.series.id
      ).then(() => {
        this.getAllComments().then(() => {});
      });
    } catch (error) {
      console.error("Erreur lors de la modification du commentaire :", error);
    }
  }

  private renderUserPart() {
    // Si l'utilisateur est connecté, on affiche la possibilité de laisser une note et un commentaire
    if (localStorage.getItem("userId") !== null) {
      return html`
        <div class="separator"></div>
        <div class="more-information-container__rating-div">
          <div class="more-information-container__error-rating">${this.errorRating}</div>
          <div class="more-information-container__actual-rating">
            <div> Note actuelle: <b>${this.series.vote_average} / 10</b></div>
            <div class="more-information-container__user-rating"> Votre note: <b>${this.userNote} / 10</b>
              ${this.userNote !== 0 ? html`<img src="corbeille.png" alt="supprimer note" @click="${this.deleteRating}">` : html``}
            </div>
            
          </div>
          <div class="more-information-container__rating-form">
            <span class="more-information-container__rating-question"> Laissez une note ? </span>
            <form id="rating-form" @submit=${this.submitRating}>
              <input type="number" min="0.5" max="10" step="0.1" name="rating" placeholder="Note (0.5-10)" required>
              <button type="submit">Envoyer</button>
            </form>
          </div>
        </div>
        <div class="separator"></div>
        <div class="more-information-container__comment-div">
          <h2> Laissez un commentaire ?</h2>
          ${this.hasAlreadyCommented ? html`<p>Vous avez déjà commenté cette série.</p>` : 
              html`
                <form id="comment-form" @submit=${this.submitComment}>
                  <textarea name="comment" placeholder="Votre commentaire" required></textarea>
                  <button type="submit">Envoyer</button>
                </form>
              `}
        </div>
        <div class="more-information-container__all-comment-div">
          <h2> Tous les commentaires sur cette série </h2>
          ${this.renderAllComments()}
        </div>`
    }
    return html``;
  }

  render() {
    if(this.series.id !== 0) {
      checkImage(this.series.backdrop_path).then((isImageValid) => {
        if (!isImageValid) {
          this.series.backdrop_path = "/src/assets/no_image.png";
        }
      });
    }
    //No img for this serie
    if (this.series.backdrop_path === "https://image.tmdb.org/t/p/w1920") {
      this.series.backdrop_path = "/src/assets/no_image.png";
    }
    return html`
            <div class="more-information-container">
                <div class="more-information-container__title-serie">
                    ${this.series.name}
                </div>
                <div class="more-information-container__information-div">
                    <div class="more-information-container__img-div" style="background-image: url(${this.series.backdrop_path});">
                    </div>
                    <div class="more-information-container__description-div">
                        <h2>Description de la série : </h2>
                        ${this.series.overview}
                    </div>
                </div>
              ${this.renderUserPart()}
            </div>
        `;
  }

  static styles = css`${unsafeCSS(MoreInformationCSS)}`;
}

declare global {
  interface HTMLElementTagNameMap {
    'more-information-component': MoreInformationComponent
  }
}
