# -------------- Table Creation Script --------------
CREATE TABLE contact(schid integer, campus varchar(60), website varchar(60), phone varchar(20), street varchar(50), city varchar(30), state varchar(5), zip varchar(15), latitude double, longitude double);
CREATE TABLE degrees(schid integer, programname varchar(60), awardname varchar(20), ciptitle varchar(60));
CREATE TABLE depsTaught(facultyid integer, deptid integer, startyear integer);
CREATE TABLE extra(schid integer, avgfin double, type varchar(60), size varchar(15), totund integer, totgrd integer, wmn double, men double, avgage double, avgagef double, part double, hisp double, asian double, white double, black double, unknown double, twormore double, nonres double, instate double, outstate double, ratio double, sat double, adtype varchar(30), admitted double);
CREATE TABLE honors(schid integer, program varchar(100), link varchar(200));
CREATE TABLE courses(deptid integer, courseno integer, coursecr double);
CREATE TABLE departments(deptid integer, schid integer, deptname varchar(8));
CREATE TABLE expenses(schid integer, resident double, transp double, bookspl double, lunch double, prsnl double, rmbrd double, credt double);
CREATE TABLE faculty(facultyid int, lastname varchar(20), firstname varchar(20), rating double, email varchar(40), startyear integer);

# -------------- Loading Data into MySQL --------------
load data local infile '/home/2020/hunter/fall/43501/dn3777/ClassProject/Data/contact.csv' into table contact fields terminated by ',' optionally enclosed by '"' ignore 1 lines;
load data local infile '/home/2020/hunter/fall/43501/dn3777/ClassProject/Data/degrees.csv' into table degrees fields terminated by ',' optionally enclosed by '"' ignore 1 lines;
load data local infile '/home/2020/hunter/fall/43501/dn3777/ClassProject/Data/depsTaught.csv' into table depsTaught fields terminated by ',' optionally enclosed by '"' ignore 1 lines;
load data local infile '/home/2020/hunter/fall/43501/dn3777/ClassProject/Data/extra.csv' into table extra fields terminated by ',' optionally enclosed by '"' ignore 1 lines;
load data local infile '/home/2020/hunter/fall/43501/dn3777/ClassProject/Data/honors.csv' into table honors fields terminated by ',' optionally enclosed by '"' ignore 1 lines;
load data local infile '/home/2020/hunter/fall/43501/dn3777/ClassProject/Data/courses.csv' into table courses fields terminated by ',' optionally enclosed by '"' ignore 1 lines;
load data local infile '/home/2020/hunter/fall/43501/dn3777/ClassProject/Data/departments.csv' into table departments fields terminated by ',' optionally enclosed by '"' ignore 1 lines;
load data local infile '/home/2020/hunter/fall/43501/dn3777/ClassProject/Data/expenses.csv' into table expenses fields terminated by ',' optionally enclosed by '"' ignore 1 lines;
load data local infile '/home/2020/hunter/fall/43501/dn3777/ClassProject/Data/faculty.csv' into table faculty fields terminated by ',' optionally enclosed by '"' ignore 1 lines;

# -------------- QUERIES ---------------

# Number of Courses per School
SELECT contact.campus Campus, B.cnt NumCourses FROM contact JOIN (SELECT schid, COUNT(*) cnt FROM (SELECT departments.schid schid, courses.courseno crs FROM courses JOIN departments ON departments.deptid = courses.deptid) A GROUP BY schid) B ON B.schid = contact.schid;

# Average Faculty Rating for Each School
SELECT contact.campus Campus, D.avg AvgRating FROM contact JOIN (SELECT schid, AVG(rating) avg FROM (SELECT A.schid schid, faculty.rating rating FROM faculty JOIN (SELECT departments.schid schid, depsTaught.facultyid factid FROM departments JOIN depsTaught ON departments.deptid = depsTaught.deptid) A ON A.factid = faculty.facultyid) C GROUP BY schid) D ON D.schid = contact.schid;

# Number of Degrees Available at Each School
SELECT contact.campus Campus, A.cnt NumDegrees FROM contact JOIN (SELECT schid, COUNT(*) cnt FROM degrees GROUP BY schid) A ON A.schid = contact.schid;

# Most Offered Course Number
SELECT courseno, cnt FROM (SELECT courseno, COUNT(*) cnt FROM courses GROUP BY courseno) A WHERE cnt = (SELECT MAX(cnt) FROM (SELECT courseno, COUNT(*) cnt FROM courses GROUP BY courseno) B);

# Average Number of Departments Across All Schools
SELECT AVG(cnt) avg FROM (SELECT schid, COUNT(*) cnt FROM departments GROUP BY schid) A;

# Total Average Expenses for Each School
SELECT contact.campus Campus, A.total Total FROM contact JOIN (SELECT schid, resident+transp+bookspl+lunch+prsnl+rmbrd+credt total FROM expenses) A ON contact.schid = A.schid;

# Percentage of Female Students At Each School
SELECT contact.campus Campus, A.wmn Women FROM contact JOIN (SELECT schid, wmn FROM extra) A ON A.schid = contact.schid ORDER BY Women DESC;

# Percentage of Female Students Across All Schools
SELECT AVG(Women) FROM (SELECT contact.campus Campus, A.wmn Women FROM contact JOIN (SELECT schid, wmn FROM extra) A ON A.schid = contact.schid) B;

# Number of Female Students At Each School
SELECT contact.campus Campus, A.TotalWomen FROM contact JOIN (SELECT schid, (totund+totgrd)*wmn TotalWomen FROM extra) A ON A.schid = contact.schid ORDER BY TotalWomen DESC;

