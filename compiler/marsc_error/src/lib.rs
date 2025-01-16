use pest::Span;
use std::fmt::{Debug, Display};

pub struct CompileError<'src, 'msg> {
    span: Span<'src>,
    msg: &'msg str,
}

impl<'src, 'msg> CompileError<'src, 'msg> {
    pub fn new(span: Span<'src>, msg: &'msg str) -> Self {
        Self { span, msg }
    }

    fn line_index(&self) -> usize {
        let input = self.span.get_input();
        let start = self.span.start();
        input[..start].chars().filter(|&c| c == '\n').count() + 1
    }

    fn column_index(&self) -> usize {
        let input = self.span.get_input();
        let start = self.span.start();

        let line_start = input[..start].rfind('\n').map_or(0, |i| i + 1);
        start - line_start + 1
    }

    fn first_line_content(&self) -> &str {
        let input = self.span.get_input();
        let start = self.span.start();

        let line_start = input[..start].rfind('\n').map_or(0, |i| i + 1);
        let line_end = input[start..].find('\n').map_or(input.len(), |i| start + i);

        &input[line_start..line_end]
    }

    fn pointer_offset(&self) -> usize {
        let input = self.span.get_input();
        let start = self.span.start();
        let line_start = input[..start].rfind('\n').map_or(0, |i| i + 1);

        start - line_start
    }
}

impl<'src, 'msg> Display for CompileError<'src, 'msg> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line_idx = self.line_index();
        let col_idx = self.column_index();
        let line_content = self.first_line_content();
        let line_number_width = line_idx.to_string().len();
        let pointer_offset = self.pointer_offset();

        writeln!(f, "--> {}:{}", line_idx, col_idx)?;
        writeln!(f, "{:>width$} |", "", width = line_number_width)?;
        writeln!(f, "{} | {}", line_idx, line_content)?;
        writeln!(
            f,
            "{:>width$} | {:>offset$}^^^^^^^^^^",
            "",
            "",
            width = line_number_width,
            offset = pointer_offset
        )?;
        writeln!(
            f,
            "{:>width$} = {}",
            "",
            self.msg,
            width = line_number_width
        )
    }
}

impl<'src, 'msg> Debug for CompileError<'src, 'msg> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
