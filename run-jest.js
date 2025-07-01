#!/usr/bin/env node

// Jest wrapper script that shows success message
const { spawn } = require('child_process');
const path = require('path');

const jestPath = path.join(__dirname, 'node_modules', 'jest', 'bin', 'jest.js');
const args = process.argv.slice(2); // Get all command line arguments except node and script name

console.log('🧪 Running Jest tests...\n');

const jest = spawn('node', [jestPath, ...args], {
  stdio: 'inherit',
  cwd: __dirname
});

jest.on('close', (code) => {
  if (code === 0) {
    console.log('\n🎉 ===============================');
    console.log('✅ All tests completed successfully!');
    console.log('🎉 ===============================\n');
  } else {
    console.log(`\n❌ Tests failed with exit code ${code}\n`);
  }
  process.exit(code);
});
