import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const config: Config = {
  title: "Langchain hs",
  tagline: 'Build LLM powered applications with Haskell',
  favicon: 'img/langchain.png',

  // Set the production url of your site here
  url: 'https://tusharad.github.io',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/langchain-hs/',
  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'tusharad', // Usually your GitHub org/user name.
  projectName: 'langchain-hs', // Usually your repo name.

  onBrokenLinks: 'ignore',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          editUrl:
            'https://github.com/tusharad/langchain-hs',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    // Replace with your project's social card
    image: 'img/langchain.png',
    navbar: {
      title: 'Langchain-hs',
      logo: {
        alt: 'Langchain-hs logo',
        src: 'img/langchain-light-back.png',
        srcDark: 'img/langchain.png',
      },
      items: [
        {
            href: "https://python.langchain.com/en/latest/",
            label: "Python Docs",
            position: "left",
          },
          {
            to: "/docs/",
            label: "Go Docs",
            position: "left",
          },
          {
            href: "https://github.com/tusharad/langchain-hs",
            label: "GitHub",
            position: "right",
          },      
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Tutorial',
              to: '/docs/category/concepts-and-examples',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'Github',
              to: 'https://github.com/tusharad/langchain-hs',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/tusharad/langchain-hs',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Tushar, Inc. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.gruvboxMaterialLight,
      darkTheme: prismThemes.gruvboxMaterialDark,
      additionalLanguages: ['haskell']
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
