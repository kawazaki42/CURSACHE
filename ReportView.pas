// Окно отчета
unit ReportView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
  StdActns,

  Dateutils,
  DBRecord;

type

  { TFormReport }

  TFormReport = class(TForm)
    ActionList: TActionList;
    ButtonClose: TButton;
    ButtonSave: TButton;
    ActionExit: TFileExit;
    ActionSaveAs: TFileSaveAs;
    Memo: TMemo;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ActionSaveAsAccept(Sender: TObject);
  private

  public

  end;

var
  FormReport: TFormReport;

procedure ShowReport(storage: TRecordList);

implementation

uses
  LazUTF8;
  //DBRecord;

// Отобразить отчет в Memo
procedure ShowReport(storage: TRecordList);
var
  header, underline, line: string;
  ndays: integer;
  rec: TRecord;
  sorted: TRecordList;
begin
  // NOTE: обычные строковые функции работают правилько только с ASCII.
  //       Необходимы UTF8-функции
  header := 'Дней до сдачи | № билета | ' +
            UTF8PadLeft('Читатель', 20) + ' | ' +
            UTF8PadLeft('Автор', 20) + ' | Книга';
  underline := UTF8PadLeft('', Length(header), '-');


  FormReport.Memo.Clear;
  sorted := TRecordList.Create;

  // Скопировать список перед сортировкой
  for rec in storage do
    sorted.InsertLast(rec);

  SortBy(sorted, fDate);
  // ShowMessage(sorted.Count.ToString);

  //FormReport.Memo1.Append('Книга Автор № билета Читатель Долг');
  FormReport.Memo.Append(header);
  FormReport.Memo.Append(underline);
  for rec in sorted do
  begin
    ndays := DaysBetween(rec.returndate, today);
    if CompareDate(rec.returndate, today) < 0 then
      ndays := -ndays;
    {line := rec.name + ' ' + rec.author + ' ' + rec.cardID + ' ' +
            rec.reader + ' ' +
            DaysBetween(rec.returndate, today).toString;}
    line := format('%13d | %s | %s | %s | %s', [
      ndays, //.toString.format + ' | ' +
      utf8padleft(rec.cardID, 8), //+ ' | ' +
      utf8padleft(rec.reader, 20), //+ ' | ' +
      utf8padleft(rec.author, 20), //+ ' | ' +
      rec.name
    ]);
    FormReport.Memo.Append( line );
  end;
  sorted.free;
end;

{$R *.lfm}

{ TFormReport }

procedure TFormReport.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormReport.ActionSaveAsAccept(Sender: TObject);
begin
  Memo.Lines.SaveToFile(ActionSaveAs.Dialog.FileName);
end;

end.

