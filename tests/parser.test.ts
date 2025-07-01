/**
 * Test file to demonstrate the COBOL AST parser
 */

import { parseCobol } from '../src/ast/parser';
import { ProgramNode } from '../src/ast/nodes';

// Sample COBOL program for testing
const sampleCobolProgram = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       AUTHOR. PROGRAMMER.
       DATE-WRITTEN. 2025-01-01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20) VALUE 'Hello, World!'.
       01 WS-COUNTER PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY WS-MESSAGE.
           MOVE 10 TO WS-COUNTER.
           PERFORM CALCULATION-PARA.
           STOP RUN.

       CALCULATION-PARA.
           DISPLAY 'Performing calculation'.
           IF WS-COUNTER > 5
               DISPLAY 'Counter is greater than 5'
           END-IF.
`;



/**
 * Function to display the AST in a tree-like format
 */
function displayASTTree(ast: ProgramNode, indent: string = ''): void {
  console.log(`${indent}📁 Program: ${ast.programName} (${ast.type})`);
  
  ast.divisions.forEach((division, index) => {
    const isLast = index === ast.divisions.length - 1;
    const divisionIndent = indent + (isLast ? '└── ' : '├── ');
    const childIndent = indent + (isLast ? '    ' : '│   ');
    
    console.log(`${divisionIndent}📂 ${division.divisionType} Division (${division.type})`);
    
    if (division.sections.length > 0) {
      division.sections.forEach((section, sectionIndex) => {
        const isSectionLast = sectionIndex === division.sections.length - 1;
        const sectionIndent = childIndent + (isSectionLast ? '└── ' : '├── ');
        const sectionChildIndent = childIndent + (isSectionLast ? '    ' : '│   ');
        
        console.log(`${sectionIndent}📄 ${section.sectionName} Section`);
        
        section.paragraphs.forEach((paragraph, paragraphIndex) => {
          const isParagraphLast = paragraphIndex === section.paragraphs.length - 1;
          const paragraphIndent = sectionChildIndent + (isParagraphLast ? '└── ' : '├── ');
          const paragraphChildIndent = sectionChildIndent + (isParagraphLast ? '    ' : '│   ');
          
          console.log(`${paragraphIndent}📝 ${paragraph.paragraphName}`);
          
          paragraph.statements.forEach((statement, statementIndex) => {
            const isStatementLast = statementIndex === paragraph.statements.length - 1;
            const statementIndent = paragraphChildIndent + (isStatementLast ? '└── ' : '├── ');
            
            console.log(`${statementIndent}⚡ ${statement.statementType} Statement`);
          });
        });
      });
    }
    
    // Handle divisions with direct paragraphs (like Procedure Division)
    if (division.type === 'ProcedureDivision') {
      const procDiv = division as any;
      if (procDiv.paragraphs && procDiv.paragraphs.length > 0) {
        procDiv.paragraphs.forEach((paragraph: any, paragraphIndex: number) => {
          const isParagraphLast = paragraphIndex === procDiv.paragraphs.length - 1;
          const paragraphIndent = childIndent + (isParagraphLast ? '└── ' : '├── ');
          const paragraphChildIndent = childIndent + (isParagraphLast ? '    ' : '│   ');
          
          console.log(`${paragraphIndent}📝 ${paragraph.paragraphName}`);
          
          paragraph.statements.forEach((statement: any, statementIndex: number) => {
            const isStatementLast = statementIndex === paragraph.statements.length - 1;
            const statementIndent = paragraphChildIndent + (isStatementLast ? '└── ' : '├── ');
            
            console.log(`${statementIndent}⚡ ${statement.statementType} Statement`);
          });
        });
      }
    }
  });
}

// Jest test suite
describe('COBOL AST Parser', () => {
  test('should parse a COBOL program and return correct AST structure', () => {
    console.log('Testing COBOL AST Parser...\n');

    // Parse the COBOL program
    const ast: ProgramNode = parseCobol(sampleCobolProgram);

    // Basic structure assertions
    expect(ast).toBeDefined();
    expect(ast.type).toBe('Program');
    expect(ast.programName).toBe('HELLO-WORLD');
    expect(ast.divisions.length).toBeGreaterThan(0);

    // Display the AST structure
    console.log('Program AST Structure:');
    console.log('======================');
    console.log(`Program Name: ${ast.programName}`);
    console.log(`Type: ${ast.type}`);
    console.log(`Number of Divisions: ${ast.divisions.length}`);

    // Display division information
    ast.divisions.forEach((division, index) => {
      console.log(`\nDivision ${index + 1}:`);
      console.log(`  Type: ${division.type}`);
      console.log(`  Division Type: ${division.divisionType}`);
      console.log(`  Number of Sections: ${division.sections.length}`);

      // Display section information
      division.sections.forEach((section, sectionIndex) => {
        console.log(`    Section ${sectionIndex + 1}:`);
        console.log(`      Name: ${section.sectionName}`);
        console.log(`      Type: ${section.type}`);
        console.log(`      Number of Paragraphs: ${section.paragraphs.length}`);

        // Display paragraph information
        section.paragraphs.forEach((paragraph, paragraphIndex) => {
          console.log(`        Paragraph ${paragraphIndex + 1}:`);
          console.log(`          Name: ${paragraph.paragraphName}`);
          console.log(`          Type: ${paragraph.type}`);
          console.log(`          Number of Statements: ${paragraph.statements.length}`);

          // Display statement information
          paragraph.statements.forEach((statement, statementIndex) => {
            console.log(`            Statement ${statementIndex + 1}:`);
            console.log(`              Type: ${statement.type}`);
            console.log(`              Statement Type: ${statement.statementType}`);
          });
        });
      });
    });

    // Display specific division details
    if (ast.identificationDivision) {
      console.log('\nIdentification Division Details:');
      console.log(`  Program ID: ${ast.identificationDivision.programId}`);
      console.log(`  Author: ${ast.identificationDivision.author || 'Not specified'}`);
      console.log(`  Date Written: ${ast.identificationDivision.dateWritten || 'Not specified'}`);
    }

    if (ast.procedureDivision) {
      console.log('\nProcedure Division Details:');
      console.log(`  Number of Sections: ${ast.procedureDivision.sections.length}`);
      console.log(`  Number of Paragraphs: ${ast.procedureDivision.paragraphs.length}`);
      console.log(`  Number of Statements: ${ast.procedureDivision.statements.length}`);

      if (ast.procedureDivision.paragraphs.length > 0) {
        console.log('\n  Paragraphs:');
        ast.procedureDivision.paragraphs.forEach((paragraph, index) => {
          console.log(`    ${index + 1}. ${paragraph.paragraphName} (${paragraph.statements.length} statements)`);
        });
      }
    }

    console.log('\n✅ Parser test completed successfully!');
  });

  test('should display AST tree visualization', () => {
    const ast = parseCobol(sampleCobolProgram);
    
    console.log('\n\n🌳 AST Tree Visualization:');
    console.log('===========================');
    
    displayASTTree(ast);
    
    expect(ast).toBeDefined();
  });
});

export { displayASTTree };
