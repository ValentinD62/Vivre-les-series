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
  idDescription: number = -1;

  @state()
  seriesList: any[] = [];

  private showDescription(event: CustomEvent) {
    this.idDescription = event.detail;
  }

  private notShowDescription(){
    this.idDescription = -1;
  }

  override firstUpdated() {
    getAllSeries().then((data) => {
        this.seriesList = data;
    })
        .catch((error) => {
        console.error("Erreur lors de la récupération des séries :", error);
    });
  }

  render() {
    return html`
            <div class="all-series-container">
                <h1>Liste des séries les mieux notés</h1>
                <div class="all-series-container__grid">
                  ${this.seriesList.map((series) => html`<series-card-component .series=${series} @showDescriptionVisible=${this.showDescription}></series-card-component>`)}
                </div>
            </div>
            ${this.idDescription !== -1 ? html`<more-information-component .series="${this.idDescription}" @notDisplayMoreInformation=${this.notShowDescription} ></more-information-component>` : ""}
        `;
  }

  static styles = css`${unsafeCSS(AllSeriesCSS)}`;
}

declare global {
  interface HTMLElementTagNameMap {
    'all-series-component': AllSeriesComponent
  }
}
