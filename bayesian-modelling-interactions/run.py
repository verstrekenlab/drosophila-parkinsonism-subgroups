import datetime
import os.path
import pandas as pd
import yaml
import numpy as np
from tqdm.auto import tqdm
from utils import (
    reformat_timestamp,
    load_trace,
    save_trace,
)  
from model import GeneInteractionModel

def today():
    return datetime.datetime.now().strftime("%Y-%m-%d")


all_data=pd.read_csv("raw/20250828_Raw_data.csv")
gene_pairs_df=pd.read_csv("raw/240627_list_gene_pairs.csv")
values=gene_pairs_df.values
all_data=all_data.loc[all_data["OUTLIER"]=="NO"]
all_data["Date_recording"]=[reformat_timestamp(t) for t in all_data["Date_recording"]]
controls=all_data.loc[all_data["Genotype"]=="W1118"].groupby("Date_recording").agg({"Depol": np.mean}).reset_index().rename({"Depol": "metric_control"}, axis=1)
all_data=all_data.merge(controls, on="Date_recording", how="left")
all_data["Double"]=all_data["Control_Genotype"]=="GI"
all_data=all_data[["Date_recording", "Genotype", "Double", "Depol", "metric_control"]].rename({"Depol": "metric"}, axis=1)
all_data["metric_norm"]=all_data["metric"]/all_data["metric_control"]
all_data["WT"]=False
all_data.loc[all_data["Genotype"]=="W1118", "WT"]=True
all_data["metric_inv"]=all_data["metric"]*-1
gene_pairs={
    i: row.tolist()
    for i, row in gene_pairs_df.iterrows()
}
all_data.to_csv("processed/genetic_interactions_database.csv")
with open("processed/gene_pairs.yaml", "w") as handle:
    yaml.dump(gene_pairs, handle, yaml.SafeDumper)


def main():

    metric="metric_norm"
    for pair_id, genes in gene_pairs.items():
        gi_genotype=genes[-1]

        if gi_genotype == "synthetic lethality":
            continue
        
        data=all_data.loc[all_data["Genotype"].isin(genes)]
        found_genes = data["Genotype"].unique()
        assert all((gene in found_genes for gene in genes)), f"Pair {pair_id}: Some gene in {genes} not included in {found_genes}" 
        
        means=data.groupby("Genotype").agg({metric: "mean"}).reset_index()
        single_mutants=means.loc[~(means["Genotype"]==gi_genotype)]
        expected_dm=np.prod(single_mutants[metric].values)
        out_folder=f"models/{today()}_models/"


    for pair_id, genes in tqdm(gene_pairs.items()):
        model_file = f"{out_folder}/model_{str(pair_id).zfill(3)}.nc"
        if os.path.exists(model_file):
            continue
        print(genes)

        try:
            data=all_data.loc[all_data["Genotype"].isin(genes )]
        
            # get the genotypes which are not the double nor the WT (i.e. the single mutants)
            single_mutant_1, single_mutant_2=data.loc[~(data["Double"]) & ~(data["WT"]), "Genotype"].unique().tolist()
            double_mutant=data.loc[data["Double"], "Genotype"].unique().tolist()
            if len(double_mutant)==1:
                double_mutant=double_mutant[0]
            else:
                print(f"Double mutant not found for pair {pair_id}: {genes}")
                continue
            
            X=pd.DataFrame(np.array([
                data["Genotype"]==single_mutant_1,
                data["Genotype"]==single_mutant_2,
                data["Genotype"]==double_mutant,
            ]).T)
            X.columns=["sm1", "sm2", "dm"]
            y=data[metric].values
                    
            model=GeneInteractionModel()
            trace=model.fit(X, y, draws=10_000)
            model.save(model_file)
            with open(f"{out_folder}/model_{str(pair_id).zfill(3)}.txt", "w") as handle:
                for gene in genes:
                    handle.write(f"{gene}\n")
            save_trace(f"{out_folder}/model_{str(pair_id).zfill(3)}.pkl", trace)
        except Exception as error:
            print(error)
            continue


if __name__ == "__main__":
    main()