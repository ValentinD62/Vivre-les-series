import {customElement, property, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import MoreInformationCSS from "./more-information.scss?inline";
import {checkImage} from "../../shared/function.ts";

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

  override connectedCallback(): void {
    super.connectedCallback();
    document.addEventListener("mousedown", this.handleOutsideClick);
  }

  override disconnectedCallback(): void {
    super.disconnectedCallback();
    document.removeEventListener("mousedown", this.handleOutsideClick);
  }

  private handleOutsideClick = (event: MouseEvent): void => {
    const target = event.target as Node;
    if (!this.contains(target)) {
      this.dispatchEvent(new CustomEvent('notDisplayMoreInformation'));
    }
  };

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
                        <!--<img src="/src/assets/test_img.png" alt="nom de la serie"/>-->
                    </div>
                    <div class="more-information-container__description-div">
                        <h2>Description de la série : </h2>
                        ${this.series.overview}
                    </div>
                </div>
                <div class="separator"></div>
                <div class="more-information-container__footer">
                    Commentaire ?
                </div>
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
