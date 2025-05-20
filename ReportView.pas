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
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    FileExit1: TFileExit;
    FileSaveAs1: TFileSaveAs;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
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

procedure ShowReport(storage: TRecordList);
var
  Header, underline: string;
  ndays: integer;
  rec: TRecord;
  line: String;
  sorted: TRecordList;
begin
  header := 'Дней до сдачи | № билета | ' +
            utf8padleft('Читатель', 20) + ' | ' +
            utf8padleft('Автор', 20) + ' | Книга';
  underline := utf8padleft('', length(header), '-');


  FormReport.Memo1.Clear;
  sorted := SortByDate(storage);
  // ShowMessage(sorted.Count.ToString);

  //FormReport.Memo1.Append('Книга Автор № билета Читатель Долг');
  FormReport.Memo1.Append(header);
  FormReport.Memo1.Append(underline);
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
    //line := DateToStr(rec.returndate);
    FormReport.Memo1.Append( line );
  end;
  sorted.free;
end;

{$R *.lfm}

{ TFormReport }

procedure TFormReport.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormReport.FileSaveAs1Accept(Sender: TObject);
begin
  Memo1.Lines.SaveToFile(FileSaveAs1.Dialog.FileName);
end;

end.

