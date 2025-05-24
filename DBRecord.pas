// Записи базы данных и действия с ними
unit DBRecord;

{$mode ObjFPC}{$H+}
{$ModeSwitch nestedprocvars}  // "Вложенные" процедуры.
                              // Необходимо для LCSVUtils

interface

uses
  Classes, SysUtils,
  GLinkedList, DateUtils,
  LCSVUtils;

type
  TRecord = record
    recordID: integer;
    name: string[255];
    author: string[255];
    cardID: string[64];
    reader: string[255];
    returndate: TDateTime;
  end;
  TRecordList = specialize TLinkedList<TRecord>;
  // Указатель на элемент списка
  PItem = TRecordList.PItem;
  // Сохраняемые в файл поля записи
  TStoredFields = (fTitle = 0, fAuthor, fCard, fReader, fDate);

function GetItem(List: TRecordList; idx: integer): TRecordList.PItem; 
function ItemByID(List: TRecordList; ID: integer): PItem;

function CompareBy(a, b: TRecord; Key: TStoredFields): Integer;  
function MinBy(List: TRecordList; Key: TStoredFields): PItem; 
procedure SortBy(var List: TRecordList; Key: TStoredFields);

procedure RenumberList(List: TRecordList);

procedure SaveToBinaryFile(storage: TRecordList; filename: string);
procedure LoadFromBinaryFile(storage: TRecordList; FileName: String);
function CSVToList(filename: string): TRecordList;
procedure ListToCSV(List: TRecordList; FileName: String);


implementation

var
  // Необходимо для вложенной процедуры
  temp: TRecordList;

// Получить элемент списка по индексу (начиная с 1)
function GetItem(List: TRecordList; idx: integer): TRecordList.PItem;
var
  cur: TRecordList.PItem;
  offset: integer;
begin
  offset := idx - 1;
  cur := List.First;
  for idx := 1 to offset do  // do (idx-1) jumps
    cur := cur^.next;
  result := cur;
end;

// Получить элемент списка по ID записи
function ItemByID(List: TRecordList; ID: integer): PItem;
var
  cur: PItem;
begin
  cur := List.First;
  while cur <> nil do
  begin
    if cur^.Data.recordID = ID then
      break;
    cur := cur^.next;
  end;
  result := cur;
end;

// Сравнить две записи по полю
function CompareBy(a, b: TRecord; Key: TStoredFields): Integer;
begin
  case Key of
    fTitle:  result := AnsiCompareText(a.name, b.name);
    fAuthor: result := AnsiCompareText(a.author, b.author);
    fCard:   result := AnsiCompareText(a.cardID, b.cardID);
    fReader: result := AnsiCompareText(a.reader, b.reader);
    fDate:   result := CompareDate(a.returndate, b.returndate);
  end;
end;

// Минимальный элемент списка по полю
function MinBy(List: TRecordList; Key: TStoredFields): PItem;
var
  tempmin, cur: TRecordList.PItem;
  a, b: TRecord;
begin
  cur := List.First;
  tempmin := cur;
  while cur <> nil do
  begin
    a := cur^.data;
    b := tempmin^.data;

    if CompareBy(a, b, Key) < 0 then
      tempmin := cur;

    cur := cur^.Next;
  end;

  result := tempmin;
end;

// Сортировать список по полю
procedure SortBy(var List: TRecordList; Key: TStoredFields);
// NOTE: меняет List (по ссылке, var)
var
  result: TRecordList;
  min: PItem;
begin
  if List.Count = 0 then exit;
  result := TRecordList.Create;

  while List.Count <> 0 do
  begin
    min := MinBy(List, Key);
    result.InsertLast(min^.data);
    List.Delete(min);
  end;

  assert(List.Count = 0);
  List.Free;
  List := result;
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

// Сохранить список в двоичный (типизированный) файл
procedure SaveToBinaryFile(storage: TRecordList; FileName: string);
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

// Загрузить список из двоичного (типизированного) файла
procedure LoadFromBinaryFile(storage: TRecordList; FileName: String);
var
  f: file of TRecord;
  rec: TRecord;
  i: integer;
begin
  AssignFile(f, FileName);
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

// Вспомогательная процедура для LoadFromCSVFile.
// Использует глобальный temp
procedure LoadRecord(Fields: TStringList);
var
  x: TRecord;
begin
  x.name := Fields[Ord(fTitle)];
  x.author := Fields[Ord(fAuthor)];
  x.cardID := Fields[Ord(fCard)];
  x.reader := Fields[Ord(fReader)];
  if not TryStrToDate(Fields[Ord(fDate)], x.returndate) then
    x.returndate := MaxDateTime;
  temp.InsertLast(x);
end;

// Получить список из текстового (CSV) файла
function CSVToList(filename: string): TRecordList;
begin
  temp := TRecordList.Create;  // Новая ссылка
  LCSVUtils.LoadFromCSVFile(FileName, @LoadRecord);
  RenumberList(temp);
  result := temp;
  temp := nil;
  // NOTE: не вызывать Free, иначе результат функции будет nil
end;


// Сохраить список в текстовый (CSV) файл
procedure ListToCSV(List: TRecordList; FileName: String);
var
  text, temp: TStringList;
  rec: TRecord;
begin
  text := TStringList.Create;
  temp := TStringList.Create;

  for rec in List do
  begin
    temp.Add(rec.name);
    temp.Add(rec.author);
    temp.Add(rec.cardID);
    temp.Add(rec.reader);
    temp.Add( DateToStr(rec.returndate) );

    text.Add(temp.CommaText);
    temp.Clear;
  end;

  text.SaveToFile(FileName);

  text.Free;
  temp.Free;
end;

end.

