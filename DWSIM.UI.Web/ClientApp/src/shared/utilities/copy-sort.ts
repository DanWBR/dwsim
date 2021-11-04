import moment from "moment";

export function _copyAndSort<T>(items: T[], columnKey: string, isSortedDescending: boolean=false): T[] {
  const key = columnKey as keyof T;
  // console.log("sorting items:", items);
  return items.slice(0).sort((a: T, b: T) => (compareItems(a[key], b[key], isSortedDescending)));
}

function compareItems(a: any, b: any, isSortedDescending: boolean): number {
  if (/^\d*(\.\d+)?$/.test(a)) {
    //  console.log("sorting numbers", a, b, isSortedDescending);
    return (isSortedDescending ? Number(a) < Number(b) : Number(a) > Number(b)) ? 1 : -1;
  }
  else if (moment(a, moment.RFC_2822).isValid()) {
    //  console.log("sorting dates", a, b, isSortedDescending);
    const first = Date.parse(a);
    const second = Date.parse(b);
    return (isSortedDescending ? first < second : first > second) ? 1 : -1;


  }
  else {
    //  console.log("sorting strings", a, b, isSortedDescending);
    return (isSortedDescending ? (a as string).toLowerCase() < (b as string).toLowerCase()
      : (a as string).toLowerCase() > (b as string).toLowerCase()) ? 1 : -1;

  }
}