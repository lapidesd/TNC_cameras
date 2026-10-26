# Charleston Mesquite (C2) training batch 1

Ten C2 images labeled with two classes. Water is any standing or moving water with a visible surface. Wet mud is saturated or damp ground that holds moisture but has no open water surface. The two classes are kept separate so the model can learn both, and they can be merged into one wet class later if that works better for the analysis.

## Folders and files
- images/  original 1280x720 JPGs renamed C2_YYYYMMDD_HHMM.jpg
- masks_water/  binary PNG, 255 = water, 0 = everything else
- masks_wetmud/  binary PNG, 255 = wet mud, 0 = everything else
- masks_classes/  one PNG per image with class values 0 = dry or other, 1 = water, 2 = wet mud (it looks black in an image viewer because the values are small)
- overlays/  blue fill = water, orange fill = wet mud, drawn over each image for review
- batch1_index.csv  one row per image with the original file name, polygon counts, and the pixel fraction of each class (plus the version 1 water fraction for comparison)
- labelstudio_import.json and label_config.xml  a Label Studio project with two Polygon Labels, water and wet_mud
- C2_batch1_before_after.jpg  all ten images on one page, with the original photo on the left and the labeled photo on the right

The two classes never overlap, and the timestamp bar at the bottom (rows 655 to 719) is 0 in every mask. The PNG masks are the ground truth for training.

## Labeling rules
- Water includes reflections, shallow brown water, floating leaves on the water, and water seen through sparse reeds or thin branches. Water fully hidden behind dense grass or branches is left out.
- Wet mud is ground that is visibly darker and smoother than the dry sand or soil next to it in the same lighting, usually along the waterline or where water recently drained.
- Dry sand, dry gravel, grass, leaf litter on dry ground, branches, logs, footprints, and tree shadows on dry ground are left out of both classes.
- Edges follow the real boundary in the photo. All boundaries were smoothed together so they are rounded rather than jagged or boxy.

## How the labels were made
- Water was traced by hand on zoomed crops and then snapped to the image with GrabCut (OpenCV) in a narrow band around each outline.
- Wet mud was made with whichever method fit the lighting in each photo. In even light, it was found by brightness, keeping ground darker than the dry sand nearby within a hand-drawn zone. In mixed light, a small classifier trained on that photo's own clearly wet and clearly dry spots was used. In heavy tree shade, where darkness is unreliable, a margin was traced along the water's edge instead.
- Every image was then checked several times at zoom, including a map of all ground darker than its surroundings and a separate independent review of every section of every image. Each spot that review flagged was checked against the photo before anything was changed.

## What each image contains
- 2023-06-21 10:00  flowing channel and upper pools as water, and the mud island between them as wet mud
- 2023-06-25 13:00  a small pool at the top right as water, and a pool at the left end of the upper strip that continues behind the tamarisk branches on the left as water, with the branches over it counted as water. Two damp strips across the channel bed are wet mud
- 2023-07-21 16:00  the channel and a pool at the top right corner as water, and the dark wet line right along both banks of the channel as wet mud
- 2023-09-12 13:00  the flood as water, including the parts seen through thin willow branches, and a narrow muddy rim along the bank as wet mud
- 2023-09-13 13:00  the channel, the small pools at the far left, and the brown water at the top right as water, and the saturated mud flat with puddles above the channel as wet mud. The sand below the channel looks dry and is left unlabeled
- 2024-07-07 10:00  the reflective pool as water, and a thin wet rim right along both edges of the pool as wet mud. The rim is narrowest where the sand at the waterline is sunlit and pale, and a little wider along the brown wet bank above the middle of the pool. The shaded ground farther from the water is left unlabeled
- 2024-07-11 13:00  no water, and a patch of darker damp sand with tracks at the upper right as wet mud
- 2024-09-05 07:00  both channels and the pools as water, including the white riffle in the left channel and a reflection under the log, and the damp sand around them as wet mud, with the pale dry sand strip in the middle, the gravel bank at the top right, and the log in the water left unlabeled
- 2024-11-03 13:00  no water, and a faint damp band that slopes across the left half of the channel bed as wet mud
- 2024-11-06 13:00  the pool, including the floating leaves on it, as water, and the damp bands above and below it as wet mud

## Feedback applied
- Ground is treated as dry unless it is visibly wet. On July 7, 2024 this means the wet mud is a thin rim at the water's edge, and the shaded ground farther from the pool is left unlabeled.
- The wet mud on the three days coded as dry (June 25, 2023, July 11, 2024, and November 3, 2024) is kept, since it may help the model tell dry ground from moist ground.
- On June 25, 2023, the water now continues to the left behind the tamarisk branches, and the branches over the pool count as water.
- The September 13, 2023 labels are unchanged.

## Remaining judgment calls
- The sandbar between the two channels on September 5, 2024 is labeled wet mud because it is dark, smooth, and surrounded by water, but it may be only damp.
- The thin brown film among the reeds on the September 13, 2023 mud flat is left as wet mud rather than water.
- Thin willow foliage over the flood on September 12, 2023 is included in the water, since the flood is visible through it.

## Note on Label Studio
Label Studio polygons cannot have holes. Where wet mud fully surrounds water (the small pools on September 13, 2023 and the left channel on September 5, 2024), the wet mud is exported as two polygons that meet along a straight cut through the pool, so the wet mud never covers the water in Label Studio. The polygons match the PNG masks to within about one pixel along the edges.

## Version history
- Version 1 (Oct 14)  smooth hand polygons, water only
- Version 2  water edges traced to the real shoreline and wet mud left out of water
- Version 3  wet mud added as a second class
- Versions 4 to 8  coverage gaps filled, edges smoothed, and repeated zoomed checks that added missed water and wet mud in every wet image
- Version 9  a full darkness map check of every image and an independent review of every section, which added damp areas on June 25, 2023, July 11, 2024, and September 5, 2024, widened the shaded damp band on July 7, 2024, added water at the top right of July 21, 2023 and September 13, 2023, refit the September 5, 2024 left channel to the riffle, made the November 3 and November 6, 2024 edges follow the photo, removed the dry-looking sand below the channel on September 13, 2023 from the wet mud class, removed a band of grass from June 21, 2023, and fixed the Label Studio export so wet mud never covers enclosed pools
- Version 10  feedback from the team applied. July 7, 2024 wet mud trimmed to a thin rim at the water's edge, June 25, 2023 water extended behind the tamarisk branches, and a small stray patch below the November 6, 2024 pool removed
