/* hw1 */
type date = (int, int, int);

type listOfDates = list(date);

/* Write a function is_older that takes two dates and evaluates to true or false.  It evaluates to true if
   the first argument is a date that comes before the second argument.  (If the two dates are the same,
   the result is false.) */
let is_older = (firstDate: date, secondDate: date) => {
  let (y1, m1, d1) = firstDate;
  let (y2, m2, d2) = secondDate;
  if (y1 < y2) {
    true;
  } else if (m1 < m2) {
    true;
  } else {
    d1 < d2;
  };
};

/* Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
   how many dates in the list are in the given month. */
let rec number_in_month = (dates: listOfDates, month: int) =>
  if (List.length(dates) == 0) {
    0;
  } else {
    let (_, m, _) = List.hd(dates);
    let sum: int = m == month ? 1 : 0;
    sum + number_in_month(List.tl(dates), month);
  };

/* Write a function number_in_months that takes a list of dates and a list of months (i.e., anint list)
   and returns the number of dates in the list of dates that are in any of the months in the list of months.
   Assume the list of months has no number repeated.
   Hint:  Use your answer to the previous problem. */
let rec number_in_months = (dates: listOfDates, months: list(int)) =>
  if (List.length(months) == 0) {
    0;
  } else {
    number_in_month(dates, List.hd(months))
    + number_in_months(dates, List.tl(months));
  };