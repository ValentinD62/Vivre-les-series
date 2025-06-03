import {customElement, property, state} from "lit/decorators.js";
import {html, css, LitElement, unsafeCSS } from "lit";
import SearchSeriesCSS from "./search-series.scss?inline";
import { getSeriesByName } from "../../API/main.ts";
import { debounce } from "../../shared/function.ts";

//Composant pour la page de recherche de série
@customElement('search-series')
export class SearchSeriesComponent extends LitElement {

  @property({ type: String, attribute: "input-value" })
  inputValue: string = "";

  @state()
  selectedSerie: any = {};

  @state()
  seriesList: any[] = [];

  @state()
  problem: boolean = false;

  @state()
  showMoreInformation: boolean = false;

  @state()
  loading: boolean = true;

  private showDescription(event: CustomEvent) {
    this.selectedSerie = event.detail;
    this.showMoreInformation = true;
  }

  private notShowDescription(){
    this.selectedSerie = {};
    this.showMoreInformation = false;
  }

  private debouncedFetchSeries = debounce((value: string) => {
    getSeriesByName(value).then((data) => {
      this.seriesList = data;
      this.problem = false;
      this.loading = false;
    }).catch((error) => {
      this.problem = true;
      this.loading = false;
      console.error("Erreur lors de la récupération des séries :", error);
    });
  }, 2000);

  override updated(changedProperties: Map<string, any>) {
    if (changedProperties.has('inputValue')) {
      this.loading = true;
      this.debouncedFetchSeries(this.inputValue);
    }
  }

  render() {
    return html`
            <div class="search-series-container">
                <div class="search-series-title">
                  ${this.loading ? html`<p class="search-series-container-loading">Chargement des séries...</p>` : html`
                    ${this.seriesList.length === 0 ? html`<p class="search-series-container-error">Il n'y a pas de séries dans votre sélection</p>` : ""}
                    ${this.problem ? html`<p class="search-series-container-error">Une erreur est survenue lors de la récupération des séries.</p>` : ""}
                  `}
                </div>
                <div class="search-series-results">
                  ${this.seriesList.map((series) => html`<series-card-component .series=${series} @showDescriptionVisible=${this.showDescription}></series-card-component>`)}
                </div>
            </div>
            ${this.showMoreInformation ? html`<more-information-component .series="${this.selectedSerie}" @notDisplayMoreInformation=${this.notShowDescription} ></more-information-component>` : ""}
        `;
  }
  static styles = css`${unsafeCSS(SearchSeriesCSS)}`;
}

declare global {
  interface HTMLElementTagNameMap {
    'search-series': SearchSeriesComponent
  }
}
