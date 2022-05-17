#required packages: steamreviews
#downloading review JSON data from Steam using Steam API
#data is saved into JSON in data folder

import steamreviews

app_ids = [1889980, 1184370, 1889981]
#1303182 Crusader Kings III: Royal Court
#1158310 Crusader Kings III
#1889980 Pathfinder: Wrath of the Righteous - Inevitable Excess
#1184370 Pathfinder: Wrath of the Righteous
#640820 Pathfinder: Kingmaker
#1889981 Pathfinder: Wrath of the Righteous - Through the Ashes

steamreviews.download_reviews_for_app_id_batch(app_ids)

