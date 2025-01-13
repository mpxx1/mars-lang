use std::fmt::Debug;

use pest::Span;

pub struct CompileError<'ast, 'msg> {
    span: Span<'ast>,
    msg: &'msg  str,
}

impl<'ast, 'msg> CompileError<'ast, 'msg> {
    pub fn new(span: Span<'ast>, msg: &'msg str) -> Self {
        Self { span, msg, }
    }

    fn line_index(&self) -> usize {
        let input = self.span.get_input();
        let start = self.span.start();
        let prefix = &input[0..start];
        prefix.chars().filter(|&c| c == '\n').count() + 1
    }

    fn line_content(&self) -> &str {
        let input = self.span.get_input();
        let start = self.span.start();
        let end = self.span.end();

        let line_start = input[..start].rfind('\n').map_or(0, |i| i + 1);
        let line_end = input[end..].find('\n').map_or(input.len(), |i| end + i);

        &input[line_start..line_end]
    }
}

impl<'ast, 'msg> Debug for CompileError<'ast, 'msg> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line_idx = self.line_index();
        let line_content = self.line_content();
        let line_number_width = line_idx.to_string().len();
        let pointer_offset = self.span.start() - line_content.as_ptr() as usize;

        writeln!(f, "Error --> {}:{}", line_idx, self.span.start())
            .unwrap();
        writeln!(f, "{:>width$} |", "", width = line_number_width)
            .unwrap();
        writeln!(f, "{} | {}", line_idx, line_content)
            .unwrap();
        writeln!(
            f,
            "{:>width$} | {pointer:>offset$}---",
            "",
            width = line_number_width,
            offset = pointer_offset,
            pointer = "^"
        ).unwrap();
        writeln!(f, " = {}", self.msg)
            .unwrap();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
