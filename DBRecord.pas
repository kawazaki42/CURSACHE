unit DBRecord;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils,
  GLinkedList, DateUtils,
  LCSVUtils;

type
  TRecord = record
    recordID: integer;
    author: string[255];
    name: string[255];
    cardID: string[64];
    //readersurname: string;
    //readerfirstname: string;
    //readerpatronym: string;
    reader: string[255];
    returndate: TDateTime;
  end;
  TRecordList = specialize TLinkedList<TRecord>;
  PItem = TRecordList.PItem;
  StoredFields = (fTitle = 0, fAuthor, fCard, fReader, fDate);

function GetItem(List: TRecordList; idx: integer): TRecordList.PItem;
function SortByDate(List: TRecordList): TRecordList;
procedure SaveToBinaryFile(storage: TRecordList; filename: string);
procedure LoadFromBinaryFile(storage: TRecordList; FileName: String);
//procedure LoadFromCSVFile(storage: TRecordList; filename: string);
function CSVToList(filename: string): TRecordList;


implementation

var
  temp: TRecordList;

function GetItem(List: TRecordList; idx: integer): TRecordList.PItem;
var
  //x: TRecord;
  cur: TRecordList.PItem;
  offset: integer;
begin
  offset := idx - 1;
  cur := List.First;
  //dec(idx);  // 1-index to 0-index; thus offset
  for idx := 1 to offset do  // do (idx-1) jumps
    cur := cur^.next;
  result := cur;
  {begin
    if idx = 0 then exit;
    dec(idx);
  end;}
end;

function MinByDate(List: TRecordList): TRecordList.PItem;
var
  tempmin, cur: TRecordList.PItem;
  //rec: TRecord;
begin
  cur := List.First;
  tempmin := List.First;
  while not cur^.IsLast do
  begin
  //for rec in List do
    if CompareDate(cur^.data.returndate, tempmin^.data.returndate) < 0 then
      tempmin := cur;
    cur := cur^.Next;
  end;
  result := tempmin
end;

function SortByDate(List: TRecordList): TRecordList;
var
  min: TRecordList.PItem;
  rec: TRecord;
  unsorted: TRecordList;
begin
  result := TRecordList.Create;
  unsorted := TRecordList.Create;
  for rec in List do
    unsorted.InsertLast(rec);

  while unsorted.Count <> 0 do
  begin
    min := MinByDate(unsorted);
    {for rec in unsorted do
      if CompareDate(rec.returndate, tempmin^.data.returndate) < 0 then
        tempmin := rec;}

    result.InsertLast(min^.data);
    unsorted.Delete(min);
  end;
  assert(unsorted.Count = 0);
  unsorted.free;
end;

procedure SaveToBinaryFile(storage: TRecordList; filename: string);
var
  f: file of TRecord;
  rec: TRecord;
begin
  AssignFile(f, filename);
  Rewrite(f);
  try
    for rec in storage do
      write(f, rec);
  finally
    CloseFile(f);
  end;
end;

procedure RenumberList(List: TRecordList);
var
  cur: PItem;
  i: integer;
begin
  if List.Count = 0 then exit;
  cur := List.First;
  i := 1;
  while cur <> nil do
  begin
    cur^.data.recordID := i;
    inc(i);
    cur := cur^.next;
  end;
end;

procedure LoadFromBinaryFile(storage: TRecordList; FileName: String);
var
  f: file of TRecord;
  rec: TRecord;
  i: integer;
begin
  AssignFile(f, filename);
  Reset(f);
  try
    i := 1;
    while not EOF(f) do
    begin
      read(f, rec);
      rec.recordID := i;
      storage.InsertLast(rec);
      inc(i);
    end;
  finally
    CloseFile(f);
  end;
end;


procedure LoadRecord(Fields: TStringList);
var
  x: TRecord;
begin
  x.name := Fields[Ord(fTitle)];
  x.author := Fields[Ord(fAuthor)];
  x.cardID := Fields[Ord(fCard)];
  x.reader := Fields[Ord(fReader)];
  x.returndate := strtodate(Fields[Ord(fDate)]);
  //x.returndate := ;
  temp.InsertLast(x);
end;

function CSVToList(filename: string): TRecordList;
begin
  //temp.free;
  temp := TRecordList.Create;  // Новая ссылка
  //result.create;
  LCSVUtils.LoadFromCSVFile(FileName, @LoadRecord);
  RenumberList(temp);
  result := temp;
end;

end.

