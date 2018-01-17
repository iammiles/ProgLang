/* hw1 */
type date = (int, int, int);

type listOfDates = list(date);

let is_older = (date_one: date, date_two: date) => {
  let (y1, m1, d1) = date_one;
  let (y2, m2, d2) = date_two;
  if (y1 < y2) {
    true;
  } else if (m1 < m2) {
    true;
  } else {
    d1 < d2;
  };
};

let rec number_in_month = (dates: listOfDates, month: int) => {
  if (List.length(dates) == 0) {
    0;
  } else {
    let (_, m, _) = List.hd(dates);
    let sum: int = m == month ? 1 : 0;
    sum + number_in_month(List.tl(dates), month);
  };
};

let rec number_in_months = (dates: listOfDates, months: list(int)) => {
  if (List.length(months) == 0) {
    0
  } else {
    number_in_month(dates, List.hd(months)) + number_in_months(dates, List.tl(months));
  }
};