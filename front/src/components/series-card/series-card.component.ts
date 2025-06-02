import {customElement, property, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import SeriesCardCSS from "./series-card.scss?inline";
import "../../components/more-information/more-information.component.ts"

@customElement('series-card-component')
export class SeriesCardComponent extends LitElement {

  @property({ type: Object, attribute: false })
  series: { id: number, name: string; vote_average: number; first_air_date: string; overview: string; backdrop_path: string } = {
    name: "",
    vote_average: 0,
    id: 0,
    first_air_date: "",
    overview: "",
    backdrop_path: ""
  };

  @state()
  isDescriptionVisible: boolean = false;

  displayMoreInformation() {
    this.isDescriptionVisible = true;
    this.dispatchEvent(new CustomEvent('showDescriptionVisible', { detail: this.series }));
  }

  render() {
    return html`
            <div class="series-card-container" @click=${this.displayMoreInformation}>
                <img src="${this.series.backdrop_path}" alt="nom de la serie"/>
                <div class="separator"></div>
                <div class="series-card-container__info-div">
                    <div class="series-card-container__info-div-title">
                        ${this.series.name}
                    </div>
                    <div class="series-card-container__info-div-more-infos">
                        ${this.series.vote_average} / 10
                    </div>
                </div>
            </div>
        `;
  }
  static styles = css`${unsafeCSS(SeriesCardCSS)}`;
}

declare global {
  interface HTMLElementTagNameMap {
    'series-card-component': SeriesCardComponent
  }
}
