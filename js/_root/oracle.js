'use strict';

const ODDS = {
    CERTAIN: 0,
    NEARLY_CERTAIN: 1,
    VERY_LIKELY: 2,
    LIKELY: 3,
    FIFTY: 4,
    UNLIKELY: 5,
    VERY_UNLIKELY: 6,
    NEARLY_IMPOSSIBLE: 7,
    IMPOSSIBLE: 8
};

const FATE_TABLE = [
    // CERTAIN
    [ [10, 50, 91], [13, 65, 94], [15, 75, 96], [17, 85, 98], [18, 90, 99], [19, 95, 100], [20, 99, 100], [20, 99, 100], [20, 99, 100] ],
    //NEARLY_CERTAIN
    [ [7, 35, 88],  [10, 50, 91], [13, 65, 94], [15, 75, 96], [17, 85, 98], [18, 90, 99],  [19, 95, 100], [20, 99, 100], [20, 99, 100] ],
    // VERY_LIKELY
    [ [5, 25, 86],  [7, 35, 88],  [10, 50, 91], [13, 65, 94], [15, 75, 96], [17, 85, 98],  [18, 90, 99],  [19, 95, 100], [20, 99, 100] ],
    // LIKELY
    [ [3, 15, 84],  [5, 25, 86],  [7, 35, 88],  [10, 50, 91], [13, 65, 94], [15, 75, 96],  [17, 85, 98],  [18, 90, 99],  [19, 95, 100] ],
    // FIFTY
    [ [2, 10, 83],  [3, 15, 84],  [5, 25, 86],  [7, 35, 88],  [10, 50, 91], [13, 65, 94],  [15, 75, 96],  [17, 85, 98],  [18, 90, 99]  ],
    // UNLIKELY
    [ [1, 5, 82],   [2, 10, 83],  [3, 15, 84],  [5, 25, 86],  [7, 35, 88],  [10, 50, 91],  [13, 65, 94],  [15, 75, 96],  [17, 85, 98]  ],
    // VERY_UNLIKELY
    [ [0, 1, 81],   [1, 5, 82],   [2, 10, 83],  [3, 15, 84],  [5, 25, 86],  [7, 35, 88],   [10, 50, 91],  [13, 65, 94],  [15, 75, 96]  ],
    // NEARLY_IMPOSSIBLE
    [ [0, 1, 81],   [0, 1, 81],   [1, 5, 82],   [2, 10, 83],  [3, 15, 84],  [5, 25, 86],   [7, 35, 88],   [10, 50, 91],  [13, 65, 94]  ],
    // IMPOSSIBLE
    [ [0, 1, 81],   [0, 1, 81],   [0, 1, 81],   [1, 5, 82],   [2, 10, 83],  [3, 15, 84],   [5, 25, 86],   [7, 35, 88],   [10, 50, 91]  ]
];


/*
  odds should be a value in ODDS like ODDS.UNLIKELY
 */
function getOdds(odds,chaos=5) {
    // chaos is between 1 and 9
    // indices are between 0 and 8
    return FATE_TABLE[odds][chaos-1]
}

function testGetOdds() {
    console.log(getOdds(ODDS.LIKELY));
    console.log(getOdds(ODDS.NEARLY_IMPOSSIBLE, 7));
}


