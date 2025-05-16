import {customElement, property, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import SeriesCardCSS from "./series-card.scss?inline";
import "../../components/more-information/more-information.component.ts"

@customElement('series-card-component')
export class SeriesCardComponent extends LitElement {

  @property({ type: Number, attribute: "series-id" })
  seriesId: number = -1;

  @state()
  isDescriptionVisible: boolean = false;

  displayMoreInformation() {
    this.isDescriptionVisible = true;
    this.dispatchEvent(new CustomEvent('showDescriptionVisible', { detail: this.seriesId }));
  }

  render() {
    return html`
            <div class="series-card-container" @click=${this.displayMoreInformation}>
                <img src="/src/assets/test_img.png" alt="nom de la serie"/>
                <div class="separator"></div>
                <div class="series-card-container__info-div">
                    <div class="series-card-container__info-div-title">
                        Le titre en fait
                    </div>
                    <div class="series-card-container__info-div-more-infos">
                        Note /5
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
