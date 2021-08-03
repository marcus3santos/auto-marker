# AUTO-MARKER
AUTO-MARKER is an automated marker for lisp program assignments where the students' assignment solutions are exported from D2L.

# Dependencies


You need to install [Steel Bank Common Lisp](http://www.sbcl.org/). For more info on how to install SBCL, [http://www.sbcl.org/getting.html](http://www.sbcl.org/getting.html)


# Installation
1. Download this repository or do `git clone [INSERT REPO URL HERE]`.
2. Compile the source lisp file **automrk.lisp** by typing the command below on a shell
  ```shell
  $ sbcl --noinform --eval '(compile-file "automrk.lisp" :output-file "automrk.fasl")' --quit
  ```
  
# Usage
### Prerequisites:
To use this tool, the following folders/files should be located in the same directory as the `automrk.lisp` file:
  1. A submissions folder containing files named as follows: `number-number - FirstName LastName - DateMonth DateNumber, YearNumber SubmissionTime AMorPM`. For example:  `~/Submissions/123456-123456 - Thor Odinson - Jan 9, 2019 3_47 PM/student-submission.lisp`. Where `/Submissions` is in the same directory as `automrk.lisp`.
  2. A test-case file containing the unit tests for the students' submissions.
  3. A D2L grades exports csv file. D2L will name such file as follows: `CourseNo - CourseName - SeasonYearNo_GradesExport_Date.csv`. For example:  `CPS607 - Robotics - W2027_GradesExport_2027-02-20-21-59.csv`.

**Note**: when generating the export file, select the following Export Options in D2L: 
- Username
- Points grade
- Last name
- First name

Below is an example of a CSV file exported by D2L:
```
Username,Last Name,First Name,Lab 0X Points Grade <Course Data>,End-of-Line-Indicator
#TTiger,Tigertongue,Tim,,#
#Patrick97,Pearson,Patrick,,#
#Towhander,Twohands,Tony,,#
#Zain1997,Zodson,Zain,,#
#Coopercat,Cooper,Cain,,#
#Hammermann,Odinson,Thor,,#
#CastleSword03,Vampireson,Alucard,,#
#TarkovIsAwesone,Handerson,Timb,,#

```
---
### Marking:

Type the following command on a shell:
```shell
$ sbcl --noinform --load automrk.fasl --eval '(mark-assignments submissions-dir is-zipped grades-export-dir test-cases-dir)' --quit
```
where 
1. `submissions-dir` is a string representing the location for the folder or zip file that holds the folders for student's submissions.
2. `is-zipped` is a boolean that informs the tool whether the `submissions-dir` is a folder or a zip file. Put `t` if the `submissions-dir` is a zip file and `nil` if it is just a regular folder. 
3. `grades-export-dir` is a string representing the location for the D2L csv grades export folder.
4. `test-cases-dir` is a string representing the location for the test cases lisp file.

Below is an example of a command for marking *unzipped* student assignments located in the folder **/Users/m3santos/Google-Drive/tmp/auto-marker/Example-assignments-folder/**:
```shell
$ sbcl --noinform --load automrk.fasl --eval '(mark-assignments  "/Users/m3santos/Google-Drive/tmp/auto-marker/Example-assignments-folder/" nil  "/Users/m3santos/Google-Drive/tmp/auto-marker/sample-D2L-report.csv"  "/Users/m3santos/Google-Drive/tmp/auto-marker/test-cases-example.lisp")' --quit
```
---
### Output
After using the tool, there would be two files generated in the same directory as the `automrk.lisp`. Those files are:
1. A log file of format `/log.csv` that will hold information about the files and their execution by the AUTO-MARKER. This information will be of this type: *In this example, the student **Thor Odinson** made a stack overflow error. Therefore, in the log file, his submission is handled as such*

|Date|Time|Full Name|Grade|Comment|Description|
|----------|-------|------------|---|--------------|-----------|
|Nov 7 2019|4:47 PM|Thor Odinson|0.0|Stack_Overflow|Control stack exhausted (no more space for function call frames).This is probably due to heavily nested or infinitely recursive functioncalls. or a tail call that SBCL cannot or has not optimized away.PROCEED WITH CAUTION.

2. A report file of format `/report.csv` that will have the same information as the D2L grades export csv file with the difference being that the grades are updated accordingly. So for the example above, `/report.csv` will be as follows:
```
Username,Last Name,First Name,Lab 0X Points Grade <Course Data>,End-of-Line-Indicator
#TTiger,Tigertongue,Tim,100.0,#
#Patrick97,Pearson,Patrick,72.5,#
#Towhander,Twohands,Tony,100.0,#
#Zain1997,Zodson,Zain,95.5,#
#Coopercat,Cooper,Cain,100.0,#
#Hammermann,Odinson,Thor,0.0,#
#CastleSword03,Vampireson,Alucard,,#
#TarkovIsAwesone,Handerson,Timb,,#

```
Note that if a student exists in the grades export but not in the submissions folder, then the grade in the `/report.csv` file would not be updated. So in this example, Alucard Vampireson and Timb Handerson did not submit a file, so their grade is not updated.

3. A Feedback folder that holds feedback files for students who have submitted a file and didn't get a full grade. The general structure is like this: Consider Timb Handerson who did not get a full grade. His feedback file will be as such:
```
- Student name: Timb Handerson

- Auto-Mark comment: OK

- Auto-Mark description:
No runtime errors

- Test cases results:
((Pass TEST-DEPOSIT (EQUAL (DEPOSIT 20) 130))
 (Pass TEST-DEPOSIT (EQUAL (DEPOSIT 10) 110))
 (Pass TEST-DEPOSIT (NOT (DEPOSIT 10001)))
 (Fail TEST-WITHDRAW (EQUAL (WITHDRAW 60) 10))
 (Pass TEST-WITHDRAW (NOT (WITHDRAW 80)))
 (Pass TEST-WITHDRAW (NOT (WITHDRAW 10001)))
 (Fail TEST-WITHDRAW (EQUAL (WITHDRAW 20) 70))
 (Fail TEST-WITHDRAW (EQUAL (WITHDRAW 10) 90)))
```

4. A zipped version of the feedback folder. To be uploaded into D2L.

# Preparing test cases
Test cases must follow a specific format in order to be used within the auto-marker. 
As an example, suppose the assignment requires the students to submit a file that includes two functions: a `fact` function that gives the factorial of a number, and a `avg` function that gives the average of a list of numbers. Then, the test cases lisp file will be something like this:
```lisp
(deftest test-fact ()
  ;; Include any global variable declarations here
  (check
    (equal (fact 5) 120)
    (equal (fact 6) 720)
    (equal (fact 7) 5040)

(deftest test-avg ()
  ;; Include any global variable declarations here
  (check
    (equal (avg '(5 8 10 2 12)) 7.4)
    (equal (avg '(0 0 0 0 0 0)) 0)
    (equal (avg '(1 2 0)) 1)

(unit-test ()
  "Calls the test cases and 'forgets' the functions that were tested."
  (test-fact)
  (fmakunbound 'fact) ; Removes the function definition from the global environment,
		      ; so the next time around the unit test is done on a freshly loaded version of this function.
  (test-avg)
  (fmakunbound 'avg))
  
(unit-test) 
```
If you notice, it is still possible to include more complex forms of tests, but the general idea is to call the function that is being tested (as it will be loaded within the lisp environment after the test case file) and check if it returns `t` when compared with the right answer.

If you wish to make test cases that do not utilize `equal`, then just make sure that a `t` is returned at the end for that particular test case if the function works as desired and `nil` if the function works falsely.

Any errors that the student could raise will be handled by the auto-marker.
# Other functions
In case you wish to mark one specific submission or test your test case file, you can use the following functions:
```
mark-std-solution (student-solution test-cases-dir)
---------------------------------------------------
Description:  Loads the student-solution file, loads the test cases, runs
              the test cases, and returns the percentage of correct results over total results

Inputs:       1) student-solution [string]: The directory for the solution of the student.
              2) test-cases-dir [string]: The directory for the test cases file. This will be used to test the solution of the students for the current assignment.

Outputs:      [list] A list of the following:
              1) [string] The grade of the student.
              2) [string] A comment that describes if there was a runtime error while loading the student submission or not
              3) [string] An edited descripting of what happened during runtime (from exceptions to conditions to whatever) that will have no #\\newline and #\\,characters
              4) [list] A readable version of the results of marking the students submission.
              5) [string] The unedited version of 3) description of what happened during runtime.

Side-effects: This function utilizes the global variable *results* while running. In the beginning by reseting it to nil, and at the end by updating it with the current
              student's submission results.
---------------------------------------------------
Usage Example: Say there was a student that you want to mark their submissions independantly from the other students. You can simply take their lisp submission file, say
               \"~/mysol.lisp\", and put it in the same folder as the \"automrk.lisp\" and the test cases lisp file \"test-cases.lisp\". Afterwards, you do as follows:

               CL-USER> (load \"automrk.lisp\") ; Loading the auto-marker into the enviroment
               CL-USER> (mark-std-solution \"mysol.lisp\" \"test-cases.lisp\") ; Calling the function to mysol.lisp
               CL-USER> (\"100.0\" \"OK\" \"No runtime errors\"
                        ((\"Pass\" TEST-DEPOSIT (EQUAL (DEPOSIT 20) 130))
                         (\"Pass\" TEST-DEPOSIT (EQUAL (DEPOSIT 10) 110))
                         (\"Pass\" TEST-DEPOSIT (NOT (DEPOSIT 10001)))
                         (\"Pass\" TEST-WITHDRAW (EQUAL (WITHDRAW 60) 10))
                         (\"Pass\" TEST-WITHDRAW (NOT (WITHDRAW 80)))
                         (\"Pass\" TEST-WITHDRAW (NOT (WITHDRAW 10001)))
                         (\"Pass\" TEST-WITHDRAW (EQUAL (WITHDRAW 20) 70))
                         (\"Pass\" TEST-WITHDRAW (EQUAL (WITHDRAW 10) 90)))
                        \"No runtime errors\")

Notes:         It is possible for the terminal to print other things according to the test cases,
               but in general, what will be returned from this function is something as seen above.
```
```
get-student-name (pathname-string)
---------------------------------------------------
Description:  Parses the pathname of a student submission directory and
              return the first and last name  of the student as a string

Inputs:       pathname-string [string]: The string version of the directory for the solution of the student.

Outputs:      [string] the full name of the student in the format \"First-name Last-name\".

Side-effects: N/A
---------------------------------------------------
Usage Example: Say that one directory for a student in the submission folder (after being unzipped) is as follows: \"486192-137409 - Alex Adams - Nov 5, 2019 102 AM\",
               then, by calling this function, you will get the string \"Alex Adams\" returned like this:

               CL-USER> (load \"automrk.lisp\") ; Loading the auto-marker into the enviroment
               CL-USER> (get-student-name \"~/submissions/486192-137409 - Alex Adams - Nov 5, 2019 102 AM/\")
               CL-USER> Alex Adams
```            
``` 
get-student-date-time (pathname-string)
---------------------------------------------------
Description:  Parses the pathname of a student submission directory and
              return the date and time of submission as a list

Inputs:       pathname-string [string]: The string version of the directory for the solution of the student.

Outputs:      [list] A list of the following:
              1) [string] The date of submission of the student in the format \"Month-name dd yyyy\"
              2) [string] The hour of submission of the student in the format \"hour-minutes [PM/AM]\"

Side-effects: N/A
---------------------------------------------------
Usage Example: Say that one directory for a student in the submission folder (after being unzipped) is as follows: \"486192-137409 - Alex Adams - Nov 5, 2019 102 AM\",
               then, by calling this function, you will get the list (\"Nov 5 2019\" \"102 AM\") returned like this:

               CL-USER> (load \"automrk.lisp\") ; Loading the auto-marker into the enviroment
               CL-USER> (get-student-date-time \"~/submissions/486192-137409 - Alex Adams - Nov 5, 2019 102 AM/\")
               CL-USER> (\"Nov 5 2019\" \"102 AM\")
```
```
AUTO-MARKER-help (func-name)
---------------------------------------------------
Description:  Provides explanation on how to use some of the functions involved with the auto-marker

Inputs:       func-name [string or symbol]: The string or symbolic representation of a function name.

Outputs:      nil

Side-effects: Will write on the terminal the complete explantation of the function placed in the argument.
---------------------------------------------------
Usage Example: Say that you want to know how to use the \"mark-std-solution\" function and want to learn more about it, then you do the following inside slime

               CL-USER> (load \"automrk.lisp\") ; Loading the auto-marker into the enviroment
               CL-USER> (AUTO-TOOL-help 'mark-std-solution) ; or (AUTO-TOOL-help \"mark-std-solution\")
               mark-std-solution (student-solution test-cases-dir)
               ---------------------------------------------------
               Description:  Loads the student-solution file ...
               ...

               CL-USER> nil
```


# Credits

- [Nabil NY Mansour](https://github.com/NabilNYMansour) - Developer
- [Professor Marcus Santos](https://github.com/marcus3santos) - Supervisor

We are grateful for the support provided by the Career Boost Program and the Department of Computer Science at Ryerson University.
