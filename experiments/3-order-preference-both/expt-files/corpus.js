// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var adjectives = _.shuffle([
		{"Predicate":"red", "Class":"color", "Chinese": "红"},
		{"Predicate":"yellow", "Class":"color", "Chinese": "黄"},
		{"Predicate":"green", "Class":"color", "Chinese": "绿"},
		{"Predicate":"blue", "Class":"color", "Chinese": "蓝"},
		{"Predicate":"purple", "Class":"color", "Chinese": "紫"},
		{"Predicate":"brown", "Class":"color", "Chinese": "棕"},											
		{"Predicate":"big", "Class":"size", "Chinese": "大"},
		{"Predicate":"small", "Class":"size", "Chinese": "小"},					
		{"Predicate":"long", "Class":"size", "Chinese": "长"},	
		{"Predicate":"short", "Class":"size", "Chinese": "短"},					
		{"Predicate":"heavy", "Class":"size", "Chinese": "重"},					
		{"Predicate":"light", "Class":"size", "Chinese": "轻"},							
		{"Predicate":"wooden", "Class":"material", "Chinese": "木制"},
		{"Predicate":"plastic", "Class":"material", "Chinese": "塑料"},
		{"Predicate":"metal", "Class":"material", "Chinese": "金属"},
		{"Predicate":"dry", "Class":"texture", "Chinese": "干"},
		{"Predicate":"hard", "Class":"texture", "Chinese": "硬"},
		{"Predicate":"soft", "Class":"texture", "Chinese": "软"},
		{"Predicate":"old", "Class":"age", "Chinese": "旧"},
		{"Predicate":"new", "Class":"age", "Chinese": "新"},
		{"Predicate":"rotten", "Class":"age", "Chinese": "腐烂"},
		{"Predicate":"fresh", "Class":"age", "Chinese": "新鲜"},
		// {"Predicate":"beautiful", "Class":"age", "Chinese": "美"},
		// {"Predicate":"wet", "Class":"texture", "Chinese": "湿"},
		{"Predicate":"good", "Class":"quality", "Chinese": "好"},
		{"Predicate":"bad", "Class":"quality", "Chinese": "坏"},
		{"Predicate":"round", "Class":"shape", "Chinese": "圆"},						
		{"Predicate":"square", "Class":"shape", "Chinese": "方"}
]);

var nouns = [
		{"Noun":"apple", "NounClass":"food", "Chinese": "苹果"},
		{"Noun":"potato", "NounClass":"food", "Chinese": "土豆"},
		{"Noun":"watermelon", "NounClass":"food", "Chinese": "西瓜"},
		{"Noun":"cheese", "NounClass":"food", "Chinese": "芝士"},
		{"Noun":"tomato", "NounClass":"food", "Chinese": "西红柿"},								
		{"Noun":"chair", "NounClass":"furniture", "Chinese": "椅子"},								
		{"Noun":"couch", "NounClass":"furniture", "Chinese": "沙发"},								
		{"Noun":"box", "NounClass":"furniture", "Chinese": "盒子"},								
		{"Noun":"bed", "NounClass":"furniture", "Chinese": "床"},								
		{"Noun":"table", "NounClass":"furniture", "Chinese": "桌子"}								
];

var stimuli =  makeStims();

function makeStims() {
	stims = [];

	while (stims.length < 26) {
		noun = _.sample(nouns);
		pred1 = _.sample(adjectives);
		pred2 = _.sample(adjectives);
		if (pred1.Class!=pred2.Class) {
			stims.push(
				{
					"Predicate1":pred1.Chinese,
					"English1":pred1.Predicate,
					"Class1":pred1.Class,	
					"Predicate2":pred2.Chinese,
					"English2":pred2.Predicate,
					"Class2":pred2.Class,			
					"EnglishNoun":noun.Noun,
					"Noun":noun.Chinese,
					"NounClass":noun.NounClass
				}			
			);
		}
	}
		
	return stims;
	
}