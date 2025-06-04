import {customElement, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import AllSeriesCSS from "./all-series.scss?inline";
import "../../components/series-card/series-card.component.ts";
import "../../components/more-information/more-information.component.ts";
import {getAllSeries} from "../../API/main.ts";

//Composant pour la page de la liste de toutes les séries
@customElement('all-series')
export class AllSeriesComponent extends LitElement {

  @state()
  selectedSerie: any = {};

  @state()
  seriesList: any[] = [];

  @state()
  problem: boolean = false;

  @state()
  showMoreInformation: boolean = false;

  private showDescription(event: CustomEvent) {
    this.selectedSerie = event.detail;
    this.showMoreInformation = true;
  }

  private notShowDescription(){
    this.selectedSerie = {};
    this.showMoreInformation = false;
  }

  override firstUpdated() {
    getAllSeries().then((data) => {
        this.seriesList = data;
    }).catch((error) => {
        this.problem = true;
        console.error("Erreur lors de la récupération des séries :", error);
    });
  }

  render() {
    return html`
            <div class="all-series-container">
                <h1>Liste des séries les mieux notés</h1>
                ${this.problem ? html`<p class="all-series-container-error">Une erreur est survenue lors de la récupération des séries.</p>` : ""}
                <div class="all-series-container__grid">
                  ${this.seriesList.map((series) => html`<series-card-component .series=${series} @showDescriptionVisible=${this.showDescription}></series-card-component>`)}
                </div>
            </div>
            ${this.showMoreInformation ? html`<more-information-component .series=${this.selectedSerie} @notDisplayMoreInformation=${this.notShowDescription} ></more-information-component>` : ""}
        `;
  }

  static styles = css`${unsafeCSS(AllSeriesCSS)}`;
}

declare global {
  interface HTMLElementTagNameMap {
    'all-series-component': AllSeriesComponent
  }
}
