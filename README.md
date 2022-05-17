# SteamGameReviews

R project for Steam Review Analysis of games/DLCs.

When I was making the Steam Review analysis I've noticed it'd be a good idea to have some template project for that.

The project is WIP and still need a lot of work on streamlining, theming and deeping down the analysis (especially, NLP part).

# Project structure

| Path                        | Description                                                                          |
|------------------------|-----------------------------------------------|
| data/                       | Folder with data where it's downloaded to. Contents gitignored                       |
| data-archive/               | Folder with the old review data in case I need it. Contents gitignored               |
| data-clean/                 | Folder with the cleaned up datasets. Contents gitignored                             |
| data-collection/            | Folder with data collection scripts. *To be changed to the more streamlined process* |
| reviews_samples/            | Folder with knitted review documents                                                 |
| snippets/                   | Folder with different small code snippets                                            |
| Data_Analysis_Log_1DLC.Rmd  | Review Analysis template for game+1DLC                                               |
| Data_Analysis_Log_2DLCs.Rmd | Review Analysis template for game+2DLCs                                              |
| Data_Cleanup_Log.Rmd        | R-markdown notebook for cleaning up the review data                                  |
| LICENSE                     | License information                                                                  |
| README.md                   | This file                                                                            |

# Process description

1.  Download Review jsons with data-collection/review_collection.py

2.  Download App summaries with data-collection/gamedata_collection.R

3.  Convert json to dataset with data-collection/review_json_to_dataset.R

4.  Check and clean data with Data_Cleanup_Log.Rmd  

5.  Use one of Data_Analysis_Log_*.Rmd to analyze datasets

# TODO

- Streamlining data collection to a simple notebook
- Finishing Data_Analysis templates
- Improve statistical analysis
- Improve NLP analysis
- Streamline the process to simple script that can be Airflowed
