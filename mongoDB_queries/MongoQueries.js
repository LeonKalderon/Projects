load("C:/data/db/prep.js")

1. db.students.aggregate([{$match:{"courses.course_status":"In Progress"}},{$group:{_id:null,count:{$sum:1}}}])
2. db.students.aggregate([{$group:{_id:"$home_city",count:{$sum:1}} } ] )

3. db.students.aggregate([{$unwind:"$hobbies"},{$group:{_id:"$hobbies",count:{$sum:1}}},{$sort:{"count":-1}}])

4. db.students.aggregate([
{$unwind:"$courses"},
{$match:{$and:[{"courses.course_status":"Complete"},{"courses.grade":{$gte:5}}]}},
{$project:{"courses.grade":1}},
{$group:{_id:"$_id",GPA:{$avg:"$courses.grade"}}},
{$sort:{"GPA":-1}},
{$limit:1}])

5. db.students.aggregate([
{$unwind:"$courses"},
{$match:{$and:[{"courses.course_status":"Complete"},{"courses.grade":10}]}},
{$project:{"courses.grade":1}},
{$group:{_id:"$_id",count:{$sum:1}}},
{$sort:{"count":-1}}])

6. db.students.aggregate([
{$unwind:"$courses"},
{$match:{$and:[{"courses.course_status":"Complete"},{"courses.grade":{$gte:5}}]}},
{$project:{"courses.grade":1,"courses.course_title":1}},
{$group:{_id:"$courses.course_title",courseAvg:{$avg:"$courses.grade"}}},
{$sort:{"courseAvg":-1}}])

7. db.students.aggregate([
{$unwind:"$courses"},
{$match:{"courses.grade":{$lt:5}}},
{$group:{_id:"$courses.course_title",countDrop:{$sum:1}}},
{$sort:{"countDrop":-1}}])

8. db.students.aggregate([
{$unwind:"$courses"},
{$match:{"courses.course_status":"Complete"}},
{$project:{_id:"$_id",codeSubstring:{$substr:["$courses.course_code",0,1]}}},
{$group:{_id:"$codeSubstring",countType:{$sum:1}}},
{$sort:{"codeSubstring":-1}}])

9. db.students.update({},
{$set:{"hobbyist":true}},
{ multi:true });
db.students.update({ $or: [{"hobbies":{$exists:false}},{ "hobbies":{$size:0}},{ "hobbies":{$size:1}},{"hobbies":{$size:2}},{"hobbies":{$size:3}}]},
{$set:{"hobbyist":false}},
{ multi:true })