import {customElement, property, state} from "lit/decorators.js";
import {html, css, LitElement, unsafeCSS } from "lit";
import SearchSeriesCSS from "./search-series.scss?inline";

//Composant pour la page de recherche de série
@customElement('search-series')
export class SearchSeriesComponent extends LitElement {
  @property({ type: Object, attribute: false })
  readonly inputData!: any;

  @property({ type: String, attribute: "input-value" })
  inputValue: string = "";

  @state()
  idDescription: number = -1;

  private showDescription(event: CustomEvent) {
    this.idDescription = event.detail;
  }

  private notShowDescription(){
    this.idDescription = -1;
  }

  render() {
    return html`
            <div class="search-series-container">
                <div class="search-series-title">
                    Résultat pour la recherche pour "${this.inputValue}"
                </div>
                <div class="search-series-results">
                    <series-card-component series-id="1" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="2" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="3" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="4" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="5" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="6" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="7" @showDescriptionVisible=${this.showDescription}></series-card-component>
                </div>
            </div>
            ${this.idDescription !== -1 ? html`<more-information-component id-series="${this.idDescription}" @notDisplayMoreInformation=${this.notShowDescription} ></more-information-component>` : ""}
        `;
  }
  static styles = css`${unsafeCSS(SearchSeriesCSS)}`;
}

declare global {
  interface HTMLElementTagNameMap {
    'search-series': SearchSeriesComponent
  }
}
